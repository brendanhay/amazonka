{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType where

import Network.AWS.Prelude

data WorkspaceDirectoryType
  = AdConnector
  | SimpleAd
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

instance FromText WorkspaceDirectoryType where
  parser =
    takeLowerText >>= \case
      "ad_connector" -> pure AdConnector
      "simple_ad" -> pure SimpleAd
      e ->
        fromTextError $
          "Failure parsing WorkspaceDirectoryType from value: '" <> e
            <> "'. Accepted values: ad_connector, simple_ad"

instance ToText WorkspaceDirectoryType where
  toText = \case
    AdConnector -> "AD_CONNECTOR"
    SimpleAd -> "SIMPLE_AD"

instance Hashable WorkspaceDirectoryType

instance NFData WorkspaceDirectoryType

instance ToByteString WorkspaceDirectoryType

instance ToQuery WorkspaceDirectoryType

instance ToHeader WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
  parseJSON = parseJSONText "WorkspaceDirectoryType"
