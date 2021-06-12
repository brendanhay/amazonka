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
-- Module      : Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause
  ( CompleteWorkflowExecutionFailedCause
      ( ..,
        CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CompleteWorkflowExecutionFailedCause = CompleteWorkflowExecutionFailedCause'
  { fromCompleteWorkflowExecutionFailedCause ::
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

pattern CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: CompleteWorkflowExecutionFailedCause
pattern CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = CompleteWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION :: CompleteWorkflowExecutionFailedCause
pattern CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION = CompleteWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION,
  CompleteWorkflowExecutionFailedCause'
  #-}
