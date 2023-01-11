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
-- Module      : Amazonka.Transcribe.Types.CallAnalyticsJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CallAnalyticsJobStatus
  ( CallAnalyticsJobStatus
      ( ..,
        CallAnalyticsJobStatus_COMPLETED,
        CallAnalyticsJobStatus_FAILED,
        CallAnalyticsJobStatus_IN_PROGRESS,
        CallAnalyticsJobStatus_QUEUED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CallAnalyticsJobStatus = CallAnalyticsJobStatus'
  { fromCallAnalyticsJobStatus ::
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

pattern CallAnalyticsJobStatus_COMPLETED :: CallAnalyticsJobStatus
pattern CallAnalyticsJobStatus_COMPLETED = CallAnalyticsJobStatus' "COMPLETED"

pattern CallAnalyticsJobStatus_FAILED :: CallAnalyticsJobStatus
pattern CallAnalyticsJobStatus_FAILED = CallAnalyticsJobStatus' "FAILED"

pattern CallAnalyticsJobStatus_IN_PROGRESS :: CallAnalyticsJobStatus
pattern CallAnalyticsJobStatus_IN_PROGRESS = CallAnalyticsJobStatus' "IN_PROGRESS"

pattern CallAnalyticsJobStatus_QUEUED :: CallAnalyticsJobStatus
pattern CallAnalyticsJobStatus_QUEUED = CallAnalyticsJobStatus' "QUEUED"

{-# COMPLETE
  CallAnalyticsJobStatus_COMPLETED,
  CallAnalyticsJobStatus_FAILED,
  CallAnalyticsJobStatus_IN_PROGRESS,
  CallAnalyticsJobStatus_QUEUED,
  CallAnalyticsJobStatus'
  #-}
