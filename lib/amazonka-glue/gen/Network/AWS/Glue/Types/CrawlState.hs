{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlState
  ( CrawlState
      ( CrawlState',
        CSRunning,
        CSCancelling,
        CSCancelled,
        CSSucceeded,
        CSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CrawlState = CrawlState' Lude.Text
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

pattern CSRunning :: CrawlState
pattern CSRunning = CrawlState' "RUNNING"

pattern CSCancelling :: CrawlState
pattern CSCancelling = CrawlState' "CANCELLING"

pattern CSCancelled :: CrawlState
pattern CSCancelled = CrawlState' "CANCELLED"

pattern CSSucceeded :: CrawlState
pattern CSSucceeded = CrawlState' "SUCCEEDED"

pattern CSFailed :: CrawlState
pattern CSFailed = CrawlState' "FAILED"

{-# COMPLETE
  CSRunning,
  CSCancelling,
  CSCancelled,
  CSSucceeded,
  CSFailed,
  CrawlState'
  #-}
