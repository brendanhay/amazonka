{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ActionType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ActionType
  = ATRecommendNodeConfig
  | ATResizeCluster
  | ATRestoreCluster
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

instance FromText ActionType where
  parser =
    takeLowerText >>= \case
      "recommend-node-config" -> pure ATRecommendNodeConfig
      "resize-cluster" -> pure ATResizeCluster
      "restore-cluster" -> pure ATRestoreCluster
      e ->
        fromTextError $
          "Failure parsing ActionType from value: '" <> e
            <> "'. Accepted values: recommend-node-config, resize-cluster, restore-cluster"

instance ToText ActionType where
  toText = \case
    ATRecommendNodeConfig -> "recommend-node-config"
    ATResizeCluster -> "resize-cluster"
    ATRestoreCluster -> "restore-cluster"

instance Hashable ActionType

instance NFData ActionType

instance ToByteString ActionType

instance ToQuery ActionType

instance ToHeader ActionType
