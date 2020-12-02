{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceTypeForTagging where

import Network.AWS.Prelude

data ResourceTypeForTagging
  = RTFTDocument
  | RTFTMaintenanceWindow
  | RTFTManagedInstance
  | RTFTOpsItem
  | RTFTParameter
  | RTFTPatchBaseline
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

instance FromText ResourceTypeForTagging where
  parser =
    takeLowerText >>= \case
      "document" -> pure RTFTDocument
      "maintenancewindow" -> pure RTFTMaintenanceWindow
      "managedinstance" -> pure RTFTManagedInstance
      "opsitem" -> pure RTFTOpsItem
      "parameter" -> pure RTFTParameter
      "patchbaseline" -> pure RTFTPatchBaseline
      e ->
        fromTextError $
          "Failure parsing ResourceTypeForTagging from value: '" <> e
            <> "'. Accepted values: document, maintenancewindow, managedinstance, opsitem, parameter, patchbaseline"

instance ToText ResourceTypeForTagging where
  toText = \case
    RTFTDocument -> "Document"
    RTFTMaintenanceWindow -> "MaintenanceWindow"
    RTFTManagedInstance -> "ManagedInstance"
    RTFTOpsItem -> "OpsItem"
    RTFTParameter -> "Parameter"
    RTFTPatchBaseline -> "PatchBaseline"

instance Hashable ResourceTypeForTagging

instance NFData ResourceTypeForTagging

instance ToByteString ResourceTypeForTagging

instance ToQuery ResourceTypeForTagging

instance ToHeader ResourceTypeForTagging

instance ToJSON ResourceTypeForTagging where
  toJSON = toJSONText
