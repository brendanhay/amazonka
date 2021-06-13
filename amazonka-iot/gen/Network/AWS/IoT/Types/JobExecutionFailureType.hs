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
-- Module      : Network.AWS.IoT.Types.JobExecutionFailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionFailureType
  ( JobExecutionFailureType
      ( ..,
        JobExecutionFailureType_ALL,
        JobExecutionFailureType_FAILED,
        JobExecutionFailureType_REJECTED,
        JobExecutionFailureType_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype JobExecutionFailureType = JobExecutionFailureType'
  { fromJobExecutionFailureType ::
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

pattern JobExecutionFailureType_ALL :: JobExecutionFailureType
pattern JobExecutionFailureType_ALL = JobExecutionFailureType' "ALL"

pattern JobExecutionFailureType_FAILED :: JobExecutionFailureType
pattern JobExecutionFailureType_FAILED = JobExecutionFailureType' "FAILED"

pattern JobExecutionFailureType_REJECTED :: JobExecutionFailureType
pattern JobExecutionFailureType_REJECTED = JobExecutionFailureType' "REJECTED"

pattern JobExecutionFailureType_TIMED_OUT :: JobExecutionFailureType
pattern JobExecutionFailureType_TIMED_OUT = JobExecutionFailureType' "TIMED_OUT"

{-# COMPLETE
  JobExecutionFailureType_ALL,
  JobExecutionFailureType_FAILED,
  JobExecutionFailureType_REJECTED,
  JobExecutionFailureType_TIMED_OUT,
  JobExecutionFailureType'
  #-}
