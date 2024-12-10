module Amazonka.Env where

import {-# SOURCE #-} Amazonka.Env.Hooks (Hooks)
import Amazonka.Logger (Logger)
import Amazonka.Prelude
import Amazonka.Types (Auth, Region, Service)
import qualified Network.HTTP.Client as Client

data Env' (withAuth :: Type -> Type) = Env
  { region :: Region,
    logger :: Logger,
    hooks :: ~Hooks,
    retryCheck :: Int -> Client.HttpException -> Bool,
    overrides :: Service -> Service,
    manager :: Client.Manager,
    auth :: withAuth Auth
  }
