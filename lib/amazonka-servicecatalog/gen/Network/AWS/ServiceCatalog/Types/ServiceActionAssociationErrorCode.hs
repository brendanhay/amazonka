{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode where

import Network.AWS.Prelude

data ServiceActionAssociationErrorCode
  = DuplicateResource
  | InternalFailure
  | LimitExceeded
  | ResourceNotFound
  | Throttling
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ServiceActionAssociationErrorCode where
  parser =
    takeLowerText >>= \case
      "duplicate_resource" -> pure DuplicateResource
      "internal_failure" -> pure InternalFailure
      "limit_exceeded" -> pure LimitExceeded
      "resource_not_found" -> pure ResourceNotFound
      "throttling" -> pure Throttling
      e ->
        fromTextError $
          "Failure parsing ServiceActionAssociationErrorCode from value: '" <> e
            <> "'. Accepted values: duplicate_resource, internal_failure, limit_exceeded, resource_not_found, throttling"

instance ToText ServiceActionAssociationErrorCode where
  toText = \case
    DuplicateResource -> "DUPLICATE_RESOURCE"
    InternalFailure -> "INTERNAL_FAILURE"
    LimitExceeded -> "LIMIT_EXCEEDED"
    ResourceNotFound -> "RESOURCE_NOT_FOUND"
    Throttling -> "THROTTLING"

instance Hashable ServiceActionAssociationErrorCode

instance NFData ServiceActionAssociationErrorCode

instance ToByteString ServiceActionAssociationErrorCode

instance ToQuery ServiceActionAssociationErrorCode

instance ToHeader ServiceActionAssociationErrorCode

instance FromJSON ServiceActionAssociationErrorCode where
  parseJSON = parseJSONText "ServiceActionAssociationErrorCode"
