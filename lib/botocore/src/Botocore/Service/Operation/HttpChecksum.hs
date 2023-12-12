{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.HttpChecksum
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.HttpChecksum where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Types (MemberName, memberName)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    enum,
    field,
    nonEmpty,
    optional,
    record,
  )
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

data Algorithm = Crc32 | Crc32C | Sha256 | Sha1
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data HttpChecksum = HttpChecksum
         { requestAlgorithmMember :: Maybe MemberName,
           requestChecksumRequired :: Maybe Bool,
           requestValidationModeMember :: Maybe MemberName,
           responseAlgorithms :: Maybe (NonEmpty Algorithm)
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e HttpChecksum
parse =
  record
    HttpChecksum
      { requestAlgorithmMember =
          optional $ field "requestAlgorithmMember" memberName,
        requestChecksumRequired =
          optional $ field "requestChecksumRequired" bool,
        requestValidationModeMember =
          optional $ field "requestValidationModeMember" memberName,
        responseAlgorithms =
          optional . field "responseAlgorithms" . nonEmpty . enum $ \case
            Crc32 -> "CRC32"
            Crc32C -> "CRC32C"
            Sha256 -> "SHA256"
            Sha1 -> "SHA1"
      }
