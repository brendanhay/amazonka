{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
  ( ESWarmPartitionInstanceType
      ( ..,
        ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch,
        ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ESWarmPartitionInstanceType = ESWarmPartitionInstanceType'
  { fromESWarmPartitionInstanceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch :: ESWarmPartitionInstanceType
pattern ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch :: ESWarmPartitionInstanceType
pattern ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch,
  ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch,
  ESWarmPartitionInstanceType'
  #-}
