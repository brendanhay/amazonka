{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.CacheClusterStatus
  ( CacheClusterStatus
    ( CacheClusterStatus'
    , CacheClusterStatusCreateInProgress
    , CacheClusterStatusAvailable
    , CacheClusterStatusDeleteInProgress
    , CacheClusterStatusNotAvailable
    , CacheClusterStatusFlushInProgress
    , fromCacheClusterStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Returns the status of the __CacheCluster__ .
newtype CacheClusterStatus = CacheClusterStatus'{fromCacheClusterStatus
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern CacheClusterStatusCreateInProgress :: CacheClusterStatus
pattern CacheClusterStatusCreateInProgress = CacheClusterStatus' "CREATE_IN_PROGRESS"

pattern CacheClusterStatusAvailable :: CacheClusterStatus
pattern CacheClusterStatusAvailable = CacheClusterStatus' "AVAILABLE"

pattern CacheClusterStatusDeleteInProgress :: CacheClusterStatus
pattern CacheClusterStatusDeleteInProgress = CacheClusterStatus' "DELETE_IN_PROGRESS"

pattern CacheClusterStatusNotAvailable :: CacheClusterStatus
pattern CacheClusterStatusNotAvailable = CacheClusterStatus' "NOT_AVAILABLE"

pattern CacheClusterStatusFlushInProgress :: CacheClusterStatus
pattern CacheClusterStatusFlushInProgress = CacheClusterStatus' "FLUSH_IN_PROGRESS"

{-# COMPLETE 
  CacheClusterStatusCreateInProgress,

  CacheClusterStatusAvailable,

  CacheClusterStatusDeleteInProgress,

  CacheClusterStatusNotAvailable,

  CacheClusterStatusFlushInProgress,
  CacheClusterStatus'
  #-}
