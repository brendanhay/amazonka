{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusStatus where

import Network.AWS.Prelude

data RadiusStatus
  = RSCompleted
  | RSCreating
  | RSFailed
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

instance FromText RadiusStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure RSCompleted
      "creating" -> pure RSCreating
      "failed" -> pure RSFailed
      e ->
        fromTextError $
          "Failure parsing RadiusStatus from value: '" <> e
            <> "'. Accepted values: completed, creating, failed"

instance ToText RadiusStatus where
  toText = \case
    RSCompleted -> "Completed"
    RSCreating -> "Creating"
    RSFailed -> "Failed"

instance Hashable RadiusStatus

instance NFData RadiusStatus

instance ToByteString RadiusStatus

instance ToQuery RadiusStatus

instance ToHeader RadiusStatus

instance FromJSON RadiusStatus where
  parseJSON = parseJSONText "RadiusStatus"
