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
-- Module      : Amazonka.FraudDetector.Types.AsyncJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.AsyncJobStatus
  ( AsyncJobStatus
      ( ..,
        AsyncJobStatus_CANCELED,
        AsyncJobStatus_CANCEL_IN_PROGRESS,
        AsyncJobStatus_COMPLETE,
        AsyncJobStatus_FAILED,
        AsyncJobStatus_IN_PROGRESS,
        AsyncJobStatus_IN_PROGRESS_INITIALIZING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AsyncJobStatus = AsyncJobStatus'
  { fromAsyncJobStatus ::
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

pattern AsyncJobStatus_CANCELED :: AsyncJobStatus
pattern AsyncJobStatus_CANCELED = AsyncJobStatus' "CANCELED"

pattern AsyncJobStatus_CANCEL_IN_PROGRESS :: AsyncJobStatus
pattern AsyncJobStatus_CANCEL_IN_PROGRESS = AsyncJobStatus' "CANCEL_IN_PROGRESS"

pattern AsyncJobStatus_COMPLETE :: AsyncJobStatus
pattern AsyncJobStatus_COMPLETE = AsyncJobStatus' "COMPLETE"

pattern AsyncJobStatus_FAILED :: AsyncJobStatus
pattern AsyncJobStatus_FAILED = AsyncJobStatus' "FAILED"

pattern AsyncJobStatus_IN_PROGRESS :: AsyncJobStatus
pattern AsyncJobStatus_IN_PROGRESS = AsyncJobStatus' "IN_PROGRESS"

pattern AsyncJobStatus_IN_PROGRESS_INITIALIZING :: AsyncJobStatus
pattern AsyncJobStatus_IN_PROGRESS_INITIALIZING = AsyncJobStatus' "IN_PROGRESS_INITIALIZING"

{-# COMPLETE
  AsyncJobStatus_CANCELED,
  AsyncJobStatus_CANCEL_IN_PROGRESS,
  AsyncJobStatus_COMPLETE,
  AsyncJobStatus_FAILED,
  AsyncJobStatus_IN_PROGRESS,
  AsyncJobStatus_IN_PROGRESS_INITIALIZING,
  AsyncJobStatus'
  #-}
