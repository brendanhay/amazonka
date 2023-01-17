module Amazonka.Env where

import Amazonka.Logger (Logger)
import Amazonka.Types (Region, Service, Auth)
import Amazonka.Prelude
import qualified Network.HTTP.Client as Client
import {-# SOURCE #-} Amazonka.Env.Hooks (Hooks)

data Env' (withAuth :: Type -> Type) = Env
  { region :: Region,
    logger :: Logger,
    hooks :: ~Hooks,
    retryCheck :: Int -> Client.HttpException -> Bool,
    overrides :: Service -> Service,
    manager :: Client.Manager,
    auth :: withAuth Auth
  }
