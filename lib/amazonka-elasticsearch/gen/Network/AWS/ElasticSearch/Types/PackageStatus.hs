{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageStatus where

import Network.AWS.Prelude

data PackageStatus
  = PSAvailable
  | PSCopyFailed
  | PSCopying
  | PSDeleteFailed
  | PSDeleted
  | PSDeleting
  | PSValidating
  | PSValidationFailed
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

instance FromText PackageStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure PSAvailable
      "copy_failed" -> pure PSCopyFailed
      "copying" -> pure PSCopying
      "delete_failed" -> pure PSDeleteFailed
      "deleted" -> pure PSDeleted
      "deleting" -> pure PSDeleting
      "validating" -> pure PSValidating
      "validation_failed" -> pure PSValidationFailed
      e ->
        fromTextError $
          "Failure parsing PackageStatus from value: '" <> e
            <> "'. Accepted values: available, copy_failed, copying, delete_failed, deleted, deleting, validating, validation_failed"

instance ToText PackageStatus where
  toText = \case
    PSAvailable -> "AVAILABLE"
    PSCopyFailed -> "COPY_FAILED"
    PSCopying -> "COPYING"
    PSDeleteFailed -> "DELETE_FAILED"
    PSDeleted -> "DELETED"
    PSDeleting -> "DELETING"
    PSValidating -> "VALIDATING"
    PSValidationFailed -> "VALIDATION_FAILED"

instance Hashable PackageStatus

instance NFData PackageStatus

instance ToByteString PackageStatus

instance ToQuery PackageStatus

instance ToHeader PackageStatus

instance FromJSON PackageStatus where
  parseJSON = parseJSONText "PackageStatus"
