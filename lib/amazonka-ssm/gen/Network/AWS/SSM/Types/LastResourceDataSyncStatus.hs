-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.LastResourceDataSyncStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LastResourceDataSyncStatus
  ( LastResourceDataSyncStatus
      ( LastResourceDataSyncStatus',
        LRDSSFailed,
        LRDSSInProgress,
        LRDSSSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus' Lude.Text
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

pattern LRDSSFailed :: LastResourceDataSyncStatus
pattern LRDSSFailed = LastResourceDataSyncStatus' "Failed"

pattern LRDSSInProgress :: LastResourceDataSyncStatus
pattern LRDSSInProgress = LastResourceDataSyncStatus' "InProgress"

pattern LRDSSSuccessful :: LastResourceDataSyncStatus
pattern LRDSSSuccessful = LastResourceDataSyncStatus' "Successful"

{-# COMPLETE
  LRDSSFailed,
  LRDSSInProgress,
  LRDSSSuccessful,
  LastResourceDataSyncStatus'
  #-}
