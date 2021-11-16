module Amazonka.HTTP (retryRequest) where

import {-# SOURCE #-} Amazonka.Auth (Env')
import Amazonka.Prelude
import Amazonka.Types

retryRequest ::
  ( MonadResource m,
    AWSRequest a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  a ->
  m (Either Error (ClientResponse (AWSResponse a)))
