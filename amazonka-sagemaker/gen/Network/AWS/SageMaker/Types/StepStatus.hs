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
import qualified Network.AWS.Prelude as Prelude

newtype StepStatus = StepStatus'
  { fromStepStatus ::
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
