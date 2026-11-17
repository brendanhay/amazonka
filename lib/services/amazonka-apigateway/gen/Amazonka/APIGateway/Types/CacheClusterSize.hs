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
-- Module      : Amazonka.APIGateway.Types.CacheClusterSize
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.CacheClusterSize
  ( CacheClusterSize
      ( ..,
        CacheClusterSize_0_5,
        CacheClusterSize_118,
        CacheClusterSize_13_5,
        CacheClusterSize_1_6,
        CacheClusterSize_237,
        CacheClusterSize_28_4,
        CacheClusterSize_58_2,
        CacheClusterSize_6_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the size of the CacheCluster.
newtype CacheClusterSize = CacheClusterSize'
  { fromCacheClusterSize ::
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

pattern CacheClusterSize_0_5 :: CacheClusterSize
pattern CacheClusterSize_0_5 = CacheClusterSize' "0.5"

pattern CacheClusterSize_118 :: CacheClusterSize
pattern CacheClusterSize_118 = CacheClusterSize' "118"

pattern CacheClusterSize_13_5 :: CacheClusterSize
pattern CacheClusterSize_13_5 = CacheClusterSize' "13.5"

pattern CacheClusterSize_1_6 :: CacheClusterSize
pattern CacheClusterSize_1_6 = CacheClusterSize' "1.6"

pattern CacheClusterSize_237 :: CacheClusterSize
pattern CacheClusterSize_237 = CacheClusterSize' "237"

pattern CacheClusterSize_28_4 :: CacheClusterSize
pattern CacheClusterSize_28_4 = CacheClusterSize' "28.4"

pattern CacheClusterSize_58_2 :: CacheClusterSize
pattern CacheClusterSize_58_2 = CacheClusterSize' "58.2"

pattern CacheClusterSize_6_1 :: CacheClusterSize
pattern CacheClusterSize_6_1 = CacheClusterSize' "6.1"

{-# COMPLETE
  CacheClusterSize_0_5,
  CacheClusterSize_118,
  CacheClusterSize_13_5,
  CacheClusterSize_1_6,
  CacheClusterSize_237,
  CacheClusterSize_28_4,
  CacheClusterSize_58_2,
  CacheClusterSize_6_1,
  CacheClusterSize'
  #-}
