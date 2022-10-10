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
-- Module      : Amazonka.SWF.Types.CompleteWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.CompleteWorkflowExecutionFailedCause
  ( CompleteWorkflowExecutionFailedCause
      ( ..,
        CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CompleteWorkflowExecutionFailedCause = CompleteWorkflowExecutionFailedCause'
  { fromCompleteWorkflowExecutionFailedCause ::
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

pattern CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: CompleteWorkflowExecutionFailedCause
pattern CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = CompleteWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION :: CompleteWorkflowExecutionFailedCause
pattern CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION = CompleteWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  CompleteWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  CompleteWorkflowExecutionFailedCause_UNHANDLED_DECISION,
  CompleteWorkflowExecutionFailedCause'
  #-}
