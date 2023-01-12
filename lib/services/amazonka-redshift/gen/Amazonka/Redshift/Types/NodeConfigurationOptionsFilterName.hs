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
-- Module      : Amazonka.Redshift.Types.NodeConfigurationOptionsFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.NodeConfigurationOptionsFilterName
  ( NodeConfigurationOptionsFilterName
      ( ..,
        NodeConfigurationOptionsFilterName_EstimatedDiskUtilizationPercent,
        NodeConfigurationOptionsFilterName_Mode,
        NodeConfigurationOptionsFilterName_NodeType,
        NodeConfigurationOptionsFilterName_NumberOfNodes
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype NodeConfigurationOptionsFilterName = NodeConfigurationOptionsFilterName'
  { fromNodeConfigurationOptionsFilterName ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
