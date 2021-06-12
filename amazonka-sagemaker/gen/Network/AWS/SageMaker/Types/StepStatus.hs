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
-- Module      : Network.AWS.SageMaker.Types.StepStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.StepStatus
  ( StepStatus
      ( ..,
        StepStatus_Executing,
        StepStatus_Failed,
        StepStatus_Starting,
        StepStatus_Stopped,
        StepStatus_Stopping,
        StepStatus_Succeeded
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StepStatus = StepStatus'
  { fromStepStatus ::
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

pattern StepStatus_Executing :: StepStatus
pattern StepStatus_Executing = StepStatus' "Executing"

pattern StepStatus_Failed :: StepStatus
pattern StepStatus_Failed = StepStatus' "Failed"

pattern StepStatus_Starting :: StepStatus
pattern StepStatus_Starting = StepStatus' "Starting"

pattern StepStatus_Stopped :: StepStatus
pattern StepStatus_Stopped = StepStatus' "Stopped"

pattern StepStatus_Stopping :: StepStatus
pattern StepStatus_Stopping = StepStatus' "Stopping"

pattern StepStatus_Succeeded :: StepStatus
pattern StepStatus_Succeeded = StepStatus' "Succeeded"

{-# COMPLETE
  StepStatus_Executing,
  StepStatus_Failed,
  StepStatus_Starting,
  StepStatus_Stopped,
  StepStatus_Stopping,
  StepStatus_Succeeded,
  StepStatus'
  #-}
