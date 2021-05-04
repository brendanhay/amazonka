{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CrawlState = CrawlState'
  { fromCrawlState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
