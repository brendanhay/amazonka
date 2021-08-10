{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
  ( NodeConfigurationOptionsFilterName
      ( ..,
        NodeConfigurationOptionsFilterName_EstimatedDiskUtilizationPercent,
        NodeConfigurationOptionsFilterName_Mode,
        NodeConfigurationOptionsFilterName_NodeType,
        NodeConfigurationOptionsFilterName_NumberOfNodes
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype NodeConfigurationOptionsFilterName = NodeConfigurationOptionsFilterName'
  { fromNodeConfigurationOptionsFilterName ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern NodeConfigurationOptionsFilterName_EstimatedDiskUtilizationPercent :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterName_EstimatedDiskUtilizationPercent = NodeConfigurationOptionsFilterName' "EstimatedDiskUtilizationPercent"

pattern NodeConfigurationOptionsFilterName_Mode :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterName_Mode = NodeConfigurationOptionsFilterName' "Mode"

pattern NodeConfigurationOptionsFilterName_NodeType :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterName_NodeType = NodeConfigurationOptionsFilterName' "NodeType"

pattern NodeConfigurationOptionsFilterName_NumberOfNodes :: NodeConfigurationOptionsFilterName
pattern NodeConfigurationOptionsFilterName_NumberOfNodes = NodeConfigurationOptionsFilterName' "NumberOfNodes"

{-# COMPLETE
  NodeConfigurationOptionsFilterName_EstimatedDiskUtilizationPercent,
  NodeConfigurationOptionsFilterName_Mode,
  NodeConfigurationOptionsFilterName_NodeType,
  NodeConfigurationOptionsFilterName_NumberOfNodes,
  NodeConfigurationOptionsFilterName'
  #-}
