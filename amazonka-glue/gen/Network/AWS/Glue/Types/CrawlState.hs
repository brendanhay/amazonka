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
-- Module      : Network.AWS.Glue.Types.CrawlState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlState
  ( CrawlState
      ( ..,
        CrawlState_CANCELLED,
        CrawlState_CANCELLING,
        CrawlState_FAILED,
        CrawlState_RUNNING,
        CrawlState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CrawlState = CrawlState'
  { fromCrawlState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern CrawlState_CANCELLED :: CrawlState
pattern CrawlState_CANCELLED = CrawlState' "CANCELLED"

pattern CrawlState_CANCELLING :: CrawlState
pattern CrawlState_CANCELLING = CrawlState' "CANCELLING"

pattern CrawlState_FAILED :: CrawlState
pattern CrawlState_FAILED = CrawlState' "FAILED"

pattern CrawlState_RUNNING :: CrawlState
pattern CrawlState_RUNNING = CrawlState' "RUNNING"

pattern CrawlState_SUCCEEDED :: CrawlState
pattern CrawlState_SUCCEEDED = CrawlState' "SUCCEEDED"

{-# COMPLETE
  CrawlState_CANCELLED,
  CrawlState_CANCELLING,
  CrawlState_FAILED,
  CrawlState_RUNNING,
  CrawlState_SUCCEEDED,
  CrawlState'
  #-}
