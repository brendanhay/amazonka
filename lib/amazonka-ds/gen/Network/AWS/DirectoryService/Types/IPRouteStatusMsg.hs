{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRouteStatusMsg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRouteStatusMsg where

import Network.AWS.Prelude

data IPRouteStatusMsg
  = AddFailed
  | Added
  | Adding
  | RemoveFailed
  | Removed
  | Removing
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

instance FromText IPRouteStatusMsg where
  parser =
    takeLowerText >>= \case
      "addfailed" -> pure AddFailed
      "added" -> pure Added
      "adding" -> pure Adding
      "removefailed" -> pure RemoveFailed
      "removed" -> pure Removed
      "removing" -> pure Removing
      e ->
        fromTextError $
          "Failure parsing IPRouteStatusMsg from value: '" <> e
            <> "'. Accepted values: addfailed, added, adding, removefailed, removed, removing"

instance ToText IPRouteStatusMsg where
  toText = \case
    AddFailed -> "AddFailed"
    Added -> "Added"
    Adding -> "Adding"
    RemoveFailed -> "RemoveFailed"
    Removed -> "Removed"
    Removing -> "Removing"

instance Hashable IPRouteStatusMsg

instance NFData IPRouteStatusMsg

instance ToByteString IPRouteStatusMsg

instance ToQuery IPRouteStatusMsg

instance ToHeader IPRouteStatusMsg

instance FromJSON IPRouteStatusMsg where
  parseJSON = parseJSONText "IPRouteStatusMsg"
