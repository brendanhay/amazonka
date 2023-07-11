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
-- Module      : Amazonka.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
  ( OpenSearchWarmPartitionInstanceType
      ( ..,
        OpenSearchWarmPartitionInstanceType_Ultrawarm1_large_search,
        OpenSearchWarmPartitionInstanceType_Ultrawarm1_medium_search,
        OpenSearchWarmPartitionInstanceType_Ultrawarm1_xlarge_search
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OpenSearchWarmPartitionInstanceType = OpenSearchWarmPartitionInstanceType'
  { fromOpenSearchWarmPartitionInstanceType ::
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

pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_large_search :: OpenSearchWarmPartitionInstanceType
pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_large_search = OpenSearchWarmPartitionInstanceType' "ultrawarm1.large.search"

pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_medium_search :: OpenSearchWarmPartitionInstanceType
pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_medium_search = OpenSearchWarmPartitionInstanceType' "ultrawarm1.medium.search"

pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_xlarge_search :: OpenSearchWarmPartitionInstanceType
pattern OpenSearchWarmPartitionInstanceType_Ultrawarm1_xlarge_search = OpenSearchWarmPartitionInstanceType' "ultrawarm1.xlarge.search"

{-# COMPLETE
  OpenSearchWarmPartitionInstanceType_Ultrawarm1_large_search,
  OpenSearchWarmPartitionInstanceType_Ultrawarm1_medium_search,
  OpenSearchWarmPartitionInstanceType_Ultrawarm1_xlarge_search,
  OpenSearchWarmPartitionInstanceType'
  #-}
