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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AsyncJobStatus = AsyncJobStatus'
  { fromAsyncJobStatus ::
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
