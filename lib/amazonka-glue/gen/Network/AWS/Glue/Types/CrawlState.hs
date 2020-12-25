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
        CrawlStateRunning,
        CrawlStateCancelling,
        CrawlStateCancelled,
        CrawlStateSucceeded,
        CrawlStateFailed,
        fromCrawlState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CrawlState = CrawlState' {fromCrawlState :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CrawlStateRunning :: CrawlState
pattern CrawlStateRunning = CrawlState' "RUNNING"

pattern CrawlStateCancelling :: CrawlState
pattern CrawlStateCancelling = CrawlState' "CANCELLING"

pattern CrawlStateCancelled :: CrawlState
pattern CrawlStateCancelled = CrawlState' "CANCELLED"

pattern CrawlStateSucceeded :: CrawlState
pattern CrawlStateSucceeded = CrawlState' "SUCCEEDED"

pattern CrawlStateFailed :: CrawlState
pattern CrawlStateFailed = CrawlState' "FAILED"

{-# COMPLETE
  CrawlStateRunning,
  CrawlStateCancelling,
  CrawlStateCancelled,
  CrawlStateSucceeded,
  CrawlStateFailed,
  CrawlState'
  #-}
