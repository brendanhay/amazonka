-- |
-- Module      : Amazonka.Auth.Exception
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Exception for errors involving AWS authentication.
module Amazonka.Auth.Exception where

import Amazonka.Core.Lens.Internal (exception, prism)
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Types

-- | An error thrown when attempting to read AuthN/AuthZ information.
data AuthError
  = RetrievalError HttpException
  | MissingEnvError Text
  | MissingFileError FilePath
  | InvalidFileError Text
  | InvalidIAMError Text
  | NotOnEC2Instance
  | CredentialChainExhausted
  deriving stock (Show, Generic)

instance Exception AuthError

instance ToLog AuthError where
  build = \case
    RetrievalError e -> build e
    MissingEnvError e -> "[MissingEnvError]  { message = " <> build e <> "}"
    MissingFileError f -> "[MissingFileError] { path = " <> build f <> "}"
    InvalidFileError e -> "[InvalidFileError] { message = " <> build e <> "}"
    InvalidIAMError e -> "[InvalidIAMError]  { message = " <> build e <> "}"
    NotOnEC2Instance -> "[NotOnEC2Instance]"
    CredentialChainExhausted -> "[CredentialChainExhausted]"

class AsAuthError a where
  -- | A general authentication error.
  _AuthError :: Prism' a AuthError

  {-# MINIMAL _AuthError #-}

  -- | An error occured while communicating over HTTP with
  -- the local metadata endpoint.
  _RetrievalError :: Prism' a HttpException

  -- | The named environment variable was not found.
  _MissingEnvError :: Prism' a Text

  -- | The specified credentials file could not be found.
  _MissingFileError :: Prism' a FilePath

  -- | An error occured parsing the credentials file.
  _InvalidFileError :: Prism' a Text

  -- | The specified IAM profile could not be found or deserialised.
  _InvalidIAMError :: Prism' a Text

  -- | Using an EC2 Instance profile was attempted not on an EC2 instance.
  _NotOnEC2Instance :: Prism' a AuthError

  _RetrievalError = _AuthError . _RetrievalError
  _MissingEnvError = _AuthError . _MissingEnvError
  _MissingFileError = _AuthError . _MissingFileError
  _InvalidFileError = _AuthError . _InvalidFileError
  _InvalidIAMError = _AuthError . _InvalidIAMError
  _NotOnEC2Instance = _AuthError . _NotOnEC2Instance

instance AsAuthError SomeException where
  _AuthError = exception

instance AsAuthError AuthError where
  _AuthError = id

  _RetrievalError = prism RetrievalError $ \case
    RetrievalError e -> Right e
    x -> Left x

  _MissingEnvError = prism MissingEnvError $ \case
    MissingEnvError e -> Right e
    x -> Left x

  _MissingFileError = prism MissingFileError $ \case
    MissingFileError f -> Right f
    x -> Left x

  _InvalidFileError = prism InvalidFileError $ \case
    InvalidFileError e -> Right e
    x -> Left x

  _InvalidIAMError = prism InvalidIAMError $ \case
    InvalidIAMError e -> Right e
    x -> Left x

  _NotOnEC2Instance = prism (const NotOnEC2Instance) $ \case
    e@NotOnEC2Instance -> Right e
    x -> Left x
