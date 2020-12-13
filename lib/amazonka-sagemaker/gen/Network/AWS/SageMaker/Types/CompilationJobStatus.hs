{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CJSInprogress,
        CJSCompleted,
        CJSFailed,
        CJSStarting,
        CJSStopping,
        CJSStopped
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

pattern CJSInprogress :: CompilationJobStatus
pattern CJSInprogress = CompilationJobStatus' "INPROGRESS"

pattern CJSCompleted :: CompilationJobStatus
pattern CJSCompleted = CompilationJobStatus' "COMPLETED"

pattern CJSFailed :: CompilationJobStatus
pattern CJSFailed = CompilationJobStatus' "FAILED"

pattern CJSStarting :: CompilationJobStatus
pattern CJSStarting = CompilationJobStatus' "STARTING"

pattern CJSStopping :: CompilationJobStatus
pattern CJSStopping = CompilationJobStatus' "STOPPING"

pattern CJSStopped :: CompilationJobStatus
pattern CJSStopped = CompilationJobStatus' "STOPPED"

{-# COMPLETE
  CJSInprogress,
  CJSCompleted,
  CJSFailed,
  CJSStarting,
  CJSStopping,
  CJSStopped,
  CompilationJobStatus'
  #-}
