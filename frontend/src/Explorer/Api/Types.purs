module Explorer.Api.Types where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Generic (class Generic, gEq, gShow)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AffjaxResponse)
import Pos.Explorer.Socket.Methods (Subscription)
import Pos.Explorer.Web.Error (ExplorerError)

type Endpoint = String

data EndpointError
    = HTTPStatusError (AffjaxResponse Json)
    | JSONDecodingError String
    | ServerError ExplorerError

instance showEndpointError :: Show EndpointError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show res.status <> " msg: " <> show res.response
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> gShow e
    show (ServerError e) =
        "ServerError: " <> gShow e

newtype RequestLimit = RequestLimit Int
newtype RequestOffset = RequestOffset Int


-- Wrapper of 'Subscription' built by 'purescript bridge'
-- needed to derive generice instances of it
newtype SocketSubscription = SocketSubscription Subscription
derive instance gSocketSubscription :: Generic SocketSubscription
derive instance newtypeSocketSubscription :: Newtype SocketSubscription _
instance eqSocketSubscription :: Eq SocketSubscription where
  eq = gEq

data SocketSubscriptionAction = KeepPrevSubscriptions | UnsubscribePrevSubscriptions
