{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.CacheClusterSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.CacheClusterSize
  ( CacheClusterSize
    ( CacheClusterSize'
    , CacheClusterSizeD0_5
    , CacheClusterSizeD1_6
    , CacheClusterSizeD6_1
    , CacheClusterSizeD13_5
    , CacheClusterSizeD28_4
    , CacheClusterSizeD58_2
    , CacheClusterSizeD118
    , CacheClusterSizeD237
    , fromCacheClusterSize
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Returns the size of the __CacheCluster__ .
newtype CacheClusterSize = CacheClusterSize'{fromCacheClusterSize
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern CacheClusterSizeD0_5 :: CacheClusterSize
pattern CacheClusterSizeD0_5 = CacheClusterSize' "0.5"

pattern CacheClusterSizeD1_6 :: CacheClusterSize
pattern CacheClusterSizeD1_6 = CacheClusterSize' "1.6"

pattern CacheClusterSizeD6_1 :: CacheClusterSize
pattern CacheClusterSizeD6_1 = CacheClusterSize' "6.1"

pattern CacheClusterSizeD13_5 :: CacheClusterSize
pattern CacheClusterSizeD13_5 = CacheClusterSize' "13.5"

pattern CacheClusterSizeD28_4 :: CacheClusterSize
pattern CacheClusterSizeD28_4 = CacheClusterSize' "28.4"

pattern CacheClusterSizeD58_2 :: CacheClusterSize
pattern CacheClusterSizeD58_2 = CacheClusterSize' "58.2"

pattern CacheClusterSizeD118 :: CacheClusterSize
pattern CacheClusterSizeD118 = CacheClusterSize' "118"

pattern CacheClusterSizeD237 :: CacheClusterSize
pattern CacheClusterSizeD237 = CacheClusterSize' "237"

{-# COMPLETE 
  CacheClusterSizeD0_5,

  CacheClusterSizeD1_6,

  CacheClusterSizeD6_1,

  CacheClusterSizeD13_5,

  CacheClusterSizeD28_4,

  CacheClusterSizeD58_2,

  CacheClusterSizeD118,

  CacheClusterSizeD237,
  CacheClusterSize'
  #-}
