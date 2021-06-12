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
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStepState
  ( RemediationExecutionStepState
      ( ..,
        RemediationExecutionStepState_FAILED,
        RemediationExecutionStepState_PENDING,
        RemediationExecutionStepState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RemediationExecutionStepState = RemediationExecutionStepState'
  { fromRemediationExecutionStepState ::
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

pattern RemediationExecutionStepState_FAILED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_FAILED = RemediationExecutionStepState' "FAILED"

pattern RemediationExecutionStepState_PENDING :: RemediationExecutionStepState
pattern RemediationExecutionStepState_PENDING = RemediationExecutionStepState' "PENDING"

pattern RemediationExecutionStepState_SUCCEEDED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_SUCCEEDED = RemediationExecutionStepState' "SUCCEEDED"

{-# COMPLETE
  RemediationExecutionStepState_FAILED,
  RemediationExecutionStepState_PENDING,
  RemediationExecutionStepState_SUCCEEDED,
  RemediationExecutionStepState'
  #-}
