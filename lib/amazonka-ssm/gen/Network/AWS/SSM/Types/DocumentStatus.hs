{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentStatus where

import Network.AWS.Prelude

-- | The status of a document.
data DocumentStatus
  = DSActive
  | DSCreating
  | DSDeleting
  | DSFailed
  | DSUpdating
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

instance FromText DocumentStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSActive
      "creating" -> pure DSCreating
      "deleting" -> pure DSDeleting
      "failed" -> pure DSFailed
      "updating" -> pure DSUpdating
      e ->
        fromTextError $
          "Failure parsing DocumentStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, failed, updating"

instance ToText DocumentStatus where
  toText = \case
    DSActive -> "Active"
    DSCreating -> "Creating"
    DSDeleting -> "Deleting"
    DSFailed -> "Failed"
    DSUpdating -> "Updating"

instance Hashable DocumentStatus

instance NFData DocumentStatus

instance ToByteString DocumentStatus

instance ToQuery DocumentStatus

instance ToHeader DocumentStatus

instance FromJSON DocumentStatus where
  parseJSON = parseJSONText "DocumentStatus"
