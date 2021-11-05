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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype OpenSearchWarmPartitionInstanceType = OpenSearchWarmPartitionInstanceType'
  { fromOpenSearchWarmPartitionInstanceType ::
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
