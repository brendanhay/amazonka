{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionStatus where

import Network.AWS.Prelude

data SchemaVersionStatus
  = SVSAvailable
  | SVSDeleting
  | SVSFailure
  | SVSPending
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

instance FromText SchemaVersionStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure SVSAvailable
      "deleting" -> pure SVSDeleting
      "failure" -> pure SVSFailure
      "pending" -> pure SVSPending
      e ->
        fromTextError $
          "Failure parsing SchemaVersionStatus from value: '" <> e
            <> "'. Accepted values: available, deleting, failure, pending"

instance ToText SchemaVersionStatus where
  toText = \case
    SVSAvailable -> "AVAILABLE"
    SVSDeleting -> "DELETING"
    SVSFailure -> "FAILURE"
    SVSPending -> "PENDING"

instance Hashable SchemaVersionStatus

instance NFData SchemaVersionStatus

instance ToByteString SchemaVersionStatus

instance ToQuery SchemaVersionStatus

instance ToHeader SchemaVersionStatus

instance FromJSON SchemaVersionStatus where
  parseJSON = parseJSONText "SchemaVersionStatus"
