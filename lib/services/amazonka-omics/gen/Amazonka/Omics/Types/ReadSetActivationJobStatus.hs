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
-- Module      : Amazonka.Omics.Types.ReadSetActivationJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetActivationJobStatus
  ( ReadSetActivationJobStatus
      ( ..,
        ReadSetActivationJobStatus_CANCELLED,
        ReadSetActivationJobStatus_CANCELLING,
        ReadSetActivationJobStatus_COMPLETED,
        ReadSetActivationJobStatus_COMPLETED_WITH_FAILURES,
        ReadSetActivationJobStatus_FAILED,
        ReadSetActivationJobStatus_IN_PROGRESS,
        ReadSetActivationJobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetActivationJobStatus = ReadSetActivationJobStatus'
  { fromReadSetActivationJobStatus ::
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

pattern ReadSetActivationJobStatus_CANCELLED :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_CANCELLED = ReadSetActivationJobStatus' "CANCELLED"

pattern ReadSetActivationJobStatus_CANCELLING :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_CANCELLING = ReadSetActivationJobStatus' "CANCELLING"

pattern ReadSetActivationJobStatus_COMPLETED :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_COMPLETED = ReadSetActivationJobStatus' "COMPLETED"

pattern ReadSetActivationJobStatus_COMPLETED_WITH_FAILURES :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_COMPLETED_WITH_FAILURES = ReadSetActivationJobStatus' "COMPLETED_WITH_FAILURES"

pattern ReadSetActivationJobStatus_FAILED :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_FAILED = ReadSetActivationJobStatus' "FAILED"

pattern ReadSetActivationJobStatus_IN_PROGRESS :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_IN_PROGRESS = ReadSetActivationJobStatus' "IN_PROGRESS"

pattern ReadSetActivationJobStatus_SUBMITTED :: ReadSetActivationJobStatus
pattern ReadSetActivationJobStatus_SUBMITTED = ReadSetActivationJobStatus' "SUBMITTED"

{-# COMPLETE
  ReadSetActivationJobStatus_CANCELLED,
  ReadSetActivationJobStatus_CANCELLING,
  ReadSetActivationJobStatus_COMPLETED,
  ReadSetActivationJobStatus_COMPLETED_WITH_FAILURES,
  ReadSetActivationJobStatus_FAILED,
  ReadSetActivationJobStatus_IN_PROGRESS,
  ReadSetActivationJobStatus_SUBMITTED,
  ReadSetActivationJobStatus'
  #-}
