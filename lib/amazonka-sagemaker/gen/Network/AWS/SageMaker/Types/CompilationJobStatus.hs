-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobStatus
  ( CompilationJobStatus
      ( CompilationJobStatus',
        CJSCompleted,
        CJSFailed,
        CJSInprogress,
        CJSStarting,
        CJSStopped,
        CJSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CompilationJobStatus = CompilationJobStatus' Lude.Text
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

pattern CJSCompleted :: CompilationJobStatus
pattern CJSCompleted = CompilationJobStatus' "COMPLETED"

pattern CJSFailed :: CompilationJobStatus
pattern CJSFailed = CompilationJobStatus' "FAILED"

pattern CJSInprogress :: CompilationJobStatus
pattern CJSInprogress = CompilationJobStatus' "INPROGRESS"

pattern CJSStarting :: CompilationJobStatus
pattern CJSStarting = CompilationJobStatus' "STARTING"

pattern CJSStopped :: CompilationJobStatus
pattern CJSStopped = CompilationJobStatus' "STOPPED"

pattern CJSStopping :: CompilationJobStatus
pattern CJSStopping = CompilationJobStatus' "STOPPING"

{-# COMPLETE
  CJSCompleted,
  CJSFailed,
  CJSInprogress,
  CJSStarting,
  CJSStopped,
  CJSStopping,
  CompilationJobStatus'
  #-}
