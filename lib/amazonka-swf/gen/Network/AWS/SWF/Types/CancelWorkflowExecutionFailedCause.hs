-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
  ( CancelWorkflowExecutionFailedCause
      ( CancelWorkflowExecutionFailedCause',
        COperationNotPermitted,
        CUnhandledDecision
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CancelWorkflowExecutionFailedCause = CancelWorkflowExecutionFailedCause' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern COperationNotPermitted :: CancelWorkflowExecutionFailedCause
pattern COperationNotPermitted = CancelWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CUnhandledDecision :: CancelWorkflowExecutionFailedCause
pattern CUnhandledDecision = CancelWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  COperationNotPermitted,
  CUnhandledDecision,
  CancelWorkflowExecutionFailedCause'
  #-}
