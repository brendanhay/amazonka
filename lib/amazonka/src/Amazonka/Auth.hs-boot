module Amazonka.Auth (Env' (..)) where

import Amazonka.Prelude
import Amazonka.Types
import Data.Monoid (Dual, Endo)
import qualified Network.HTTP.Client as Client

data Env' withAuth = Env
  { _envRegion :: Region,
    _envLogger :: Logger,
    _envRetryCheck :: Int -> Client.HttpException -> Bool,
    _envOverride :: Dual (Endo Service),
    _envManager :: Client.Manager,
    _envAuth :: withAuth Auth
  }
