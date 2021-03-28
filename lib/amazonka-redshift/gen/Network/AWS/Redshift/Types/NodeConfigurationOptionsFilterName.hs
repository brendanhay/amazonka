{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
  ( NodeConfigurationOptionsFilterName
    ( NodeConfigurationOptionsFilterName'
    , NodeConfigurationOptionsFilterNameNodeType
    , NodeConfigurationOptionsFilterNameNumberOfNodes
    , NodeConfigurationOptionsFilterNameEstimatedDiskUtilizationPercent
    , NodeConfigurationOptionsFilterNameMode
    , fromNodeConfigurationOptionsFilterName
    )
  ) where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

newtype NodeConfigurationOptionsFilterName = NodeConfigurationOptionsFilterName'{fromNodeConfigurationOptionsFilterName
                                                                                 :: Core.Text}
                                               deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                               Core.Show, Core.Generic)
                                               deriving newtype (Core.IsString, Core.Hashable,
                                                                 Core.NFData, Core.ToJSONKey,
                                                                 Core.FromJSONKey, Core.ToJSON,
                                                                 Core.FromJSON, Core.ToXML,
                                                                 Core.FromXML, Core.ToText,
                                                                 Core.FromText, Core.ToByteString,
                                                                 Core.ToQuery, Core.ToHeader)

pattern NodeConfigurationOptionsFilterNameNodeType :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterNameNodeType = NodeConfigurationOptionsFilterName' "NodeType"

pattern NodeConfigurationOptionsFilterNameNumberOfNodes :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterNameNumberOfNodes = NodeConfigurationOptionsFilterName' "NumberOfNodes"

pattern NodeConfigurationOptionsFilterNameEstimatedDiskUtilizationPercent :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterNameEstimatedDiskUtilizationPercent = NodeConfigurationOptionsFilterName' "EstimatedDiskUtilizationPercent"

pattern NodeConfigurationOptionsFilterNameMode :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterNameMode = NodeConfigurationOptionsFilterName' "Mode"

{-# COMPLETE 
  NodeConfigurationOptionsFilterNameNodeType,

  NodeConfigurationOptionsFilterNameNumberOfNodes,

  NodeConfigurationOptionsFilterNameEstimatedDiskUtilizationPercent,

  NodeConfigurationOptionsFilterNameMode,
  NodeConfigurationOptionsFilterName'
  #-}
