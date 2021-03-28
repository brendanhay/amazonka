{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LastCrawlStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.LastCrawlStatus
  ( LastCrawlStatus
    ( LastCrawlStatus'
    , LastCrawlStatusSucceeded
    , LastCrawlStatusCancelled
    , LastCrawlStatusFailed
    , fromLastCrawlStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LastCrawlStatus = LastCrawlStatus'{fromLastCrawlStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern LastCrawlStatusSucceeded :: LastCrawlStatus
pattern LastCrawlStatusSucceeded = LastCrawlStatus' "SUCCEEDED"

pattern LastCrawlStatusCancelled :: LastCrawlStatus
pattern LastCrawlStatusCancelled = LastCrawlStatus' "CANCELLED"

pattern LastCrawlStatusFailed :: LastCrawlStatus
pattern LastCrawlStatusFailed = LastCrawlStatus' "FAILED"

{-# COMPLETE 
  LastCrawlStatusSucceeded,

  LastCrawlStatusCancelled,

  LastCrawlStatusFailed,
  LastCrawlStatus'
  #-}
