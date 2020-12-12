{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
  ( ESWarmPartitionInstanceType
      ( ESWarmPartitionInstanceType',
        ESWPITULTRAWARM1_Large_Elasticsearch,
        ESWPITULTRAWARM1_Medium_Elasticsearch
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ESWarmPartitionInstanceType = ESWarmPartitionInstanceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ESWPITULTRAWARM1_Large_Elasticsearch :: ESWarmPartitionInstanceType
pattern ESWPITULTRAWARM1_Large_Elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESWPITULTRAWARM1_Medium_Elasticsearch :: ESWarmPartitionInstanceType
pattern ESWPITULTRAWARM1_Medium_Elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  ESWPITULTRAWARM1_Large_Elasticsearch,
  ESWPITULTRAWARM1_Medium_Elasticsearch,
  ESWarmPartitionInstanceType'
  #-}
