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
-- Module      : Amazonka.Glue.Types.CrawlerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerState
  ( CrawlerState
      ( ..,
        CrawlerState_READY,
        CrawlerState_RUNNING,
        CrawlerState_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CrawlerState = CrawlerState'
  { fromCrawlerState ::
      Core.Text
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

pattern CrawlerState_READY :: CrawlerState
pattern CrawlerState_READY = CrawlerState' "READY"

pattern CrawlerState_RUNNING :: CrawlerState
pattern CrawlerState_RUNNING = CrawlerState' "RUNNING"

pattern CrawlerState_STOPPING :: CrawlerState
pattern CrawlerState_STOPPING = CrawlerState' "STOPPING"

{-# COMPLETE
  CrawlerState_READY,
  CrawlerState_RUNNING,
  CrawlerState_STOPPING,
  CrawlerState'
  #-}
