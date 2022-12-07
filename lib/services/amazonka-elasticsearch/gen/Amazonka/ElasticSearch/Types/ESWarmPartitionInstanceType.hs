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
-- Module      : Amazonka.ElasticSearch.Types.ESWarmPartitionInstanceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ESWarmPartitionInstanceType
  ( ESWarmPartitionInstanceType
      ( ..,
        ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch,
        ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ESWarmPartitionInstanceType = ESWarmPartitionInstanceType'
  { fromESWarmPartitionInstanceType ::
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

pattern ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch :: ESWarmPartitionInstanceType
pattern ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch :: ESWarmPartitionInstanceType
pattern ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  ESWarmPartitionInstanceType_Ultrawarm1_large_elasticsearch,
  ESWarmPartitionInstanceType_Ultrawarm1_medium_elasticsearch,
  ESWarmPartitionInstanceType'
  #-}
