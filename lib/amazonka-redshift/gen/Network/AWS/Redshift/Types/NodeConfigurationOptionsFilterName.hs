{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data NodeConfigurationOptionsFilterName
  = EstimatedDiskUtilizationPercent
  | Mode
  | NodeType
  | NumberOfNodes
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

instance FromText NodeConfigurationOptionsFilterName where
  parser =
    takeLowerText >>= \case
      "estimateddiskutilizationpercent" -> pure EstimatedDiskUtilizationPercent
      "mode" -> pure Mode
      "nodetype" -> pure NodeType
      "numberofnodes" -> pure NumberOfNodes
      e ->
        fromTextError $
          "Failure parsing NodeConfigurationOptionsFilterName from value: '" <> e
            <> "'. Accepted values: estimateddiskutilizationpercent, mode, nodetype, numberofnodes"

instance ToText NodeConfigurationOptionsFilterName where
  toText = \case
    EstimatedDiskUtilizationPercent -> "EstimatedDiskUtilizationPercent"
    Mode -> "Mode"
    NodeType -> "NodeType"
    NumberOfNodes -> "NumberOfNodes"

instance Hashable NodeConfigurationOptionsFilterName

instance NFData NodeConfigurationOptionsFilterName

instance ToByteString NodeConfigurationOptionsFilterName

instance ToQuery NodeConfigurationOptionsFilterName

instance ToHeader NodeConfigurationOptionsFilterName
