{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CacheClusterStatus
  ( CacheClusterStatus
      ( CacheClusterStatus',
        CreateInProgress,
        Available,
        DeleteInProgress,
        NotAvailable,
        FlushInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Returns the status of the __CacheCluster__ .
newtype CacheClusterStatus = CacheClusterStatus' Lude.Text
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

pattern CreateInProgress :: CacheClusterStatus
pattern CreateInProgress = CacheClusterStatus' "CREATE_IN_PROGRESS"

pattern Available :: CacheClusterStatus
pattern Available = CacheClusterStatus' "AVAILABLE"

pattern DeleteInProgress :: CacheClusterStatus
pattern DeleteInProgress = CacheClusterStatus' "DELETE_IN_PROGRESS"

pattern NotAvailable :: CacheClusterStatus
pattern NotAvailable = CacheClusterStatus' "NOT_AVAILABLE"

pattern FlushInProgress :: CacheClusterStatus
pattern FlushInProgress = CacheClusterStatus' "FLUSH_IN_PROGRESS"

{-# COMPLETE
  CreateInProgress,
  Available,
  DeleteInProgress,
  NotAvailable,
  FlushInProgress,
  CacheClusterStatus'
  #-}
