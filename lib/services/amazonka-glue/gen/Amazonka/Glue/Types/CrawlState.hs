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
-- Module      : Amazonka.Glue.Types.CrawlState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlState
  ( CrawlState
      ( ..,
        CrawlState_CANCELLED,
        CrawlState_CANCELLING,
        CrawlState_ERROR,
        CrawlState_FAILED,
        CrawlState_RUNNING,
        CrawlState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CrawlState = CrawlState'
  { fromCrawlState ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CrawlState_CANCELLED :: CrawlState
pattern CrawlState_CANCELLED = CrawlState' "CANCELLED"

pattern CrawlState_CANCELLING :: CrawlState
pattern CrawlState_CANCELLING = CrawlState' "CANCELLING"

pattern CrawlState_ERROR :: CrawlState
pattern CrawlState_ERROR = CrawlState' "ERROR"

pattern CrawlState_FAILED :: CrawlState
pattern CrawlState_FAILED = CrawlState' "FAILED"

pattern CrawlState_RUNNING :: CrawlState
pattern CrawlState_RUNNING = CrawlState' "RUNNING"

pattern CrawlState_SUCCEEDED :: CrawlState
pattern CrawlState_SUCCEEDED = CrawlState' "SUCCEEDED"

{-# COMPLETE
  CrawlState_CANCELLED,
  CrawlState_CANCELLING,
  CrawlState_ERROR,
  CrawlState_FAILED,
  CrawlState_RUNNING,
  CrawlState_SUCCEEDED,
  CrawlState'
  #-}
