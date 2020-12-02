{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaStatus where

import Network.AWS.Prelude

data SchemaStatus
  = SSAvailable
  | SSDeleting
  | SSPending
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

instance FromText SchemaStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure SSAvailable
      "deleting" -> pure SSDeleting
      "pending" -> pure SSPending
      e ->
        fromTextError $
          "Failure parsing SchemaStatus from value: '" <> e
            <> "'. Accepted values: available, deleting, pending"

instance ToText SchemaStatus where
  toText = \case
    SSAvailable -> "AVAILABLE"
    SSDeleting -> "DELETING"
    SSPending -> "PENDING"

instance Hashable SchemaStatus

instance NFData SchemaStatus

instance ToByteString SchemaStatus

instance ToQuery SchemaStatus

instance ToHeader SchemaStatus

instance FromJSON SchemaStatus where
  parseJSON = parseJSONText "SchemaStatus"
