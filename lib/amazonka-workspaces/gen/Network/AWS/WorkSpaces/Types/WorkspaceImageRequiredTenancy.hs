{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy where

import Network.AWS.Prelude

data WorkspaceImageRequiredTenancy
  = WIRTDedicated
  | WIRTDefault
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

instance FromText WorkspaceImageRequiredTenancy where
  parser =
    takeLowerText >>= \case
      "dedicated" -> pure WIRTDedicated
      "default" -> pure WIRTDefault
      e ->
        fromTextError $
          "Failure parsing WorkspaceImageRequiredTenancy from value: '" <> e
            <> "'. Accepted values: dedicated, default"

instance ToText WorkspaceImageRequiredTenancy where
  toText = \case
    WIRTDedicated -> "DEDICATED"
    WIRTDefault -> "DEFAULT"

instance Hashable WorkspaceImageRequiredTenancy

instance NFData WorkspaceImageRequiredTenancy

instance ToByteString WorkspaceImageRequiredTenancy

instance ToQuery WorkspaceImageRequiredTenancy

instance ToHeader WorkspaceImageRequiredTenancy

instance FromJSON WorkspaceImageRequiredTenancy where
  parseJSON = parseJSONText "WorkspaceImageRequiredTenancy"
