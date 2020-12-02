{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.TargetWorkspaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.TargetWorkspaceState where

import Network.AWS.Prelude

data TargetWorkspaceState
  = AdminMaintenance
  | Available
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

instance FromText TargetWorkspaceState where
  parser =
    takeLowerText >>= \case
      "admin_maintenance" -> pure AdminMaintenance
      "available" -> pure Available
      e ->
        fromTextError $
          "Failure parsing TargetWorkspaceState from value: '" <> e
            <> "'. Accepted values: admin_maintenance, available"

instance ToText TargetWorkspaceState where
  toText = \case
    AdminMaintenance -> "ADMIN_MAINTENANCE"
    Available -> "AVAILABLE"

instance Hashable TargetWorkspaceState

instance NFData TargetWorkspaceState

instance ToByteString TargetWorkspaceState

instance ToQuery TargetWorkspaceState

instance ToHeader TargetWorkspaceState

instance ToJSON TargetWorkspaceState where
  toJSON = toJSONText
