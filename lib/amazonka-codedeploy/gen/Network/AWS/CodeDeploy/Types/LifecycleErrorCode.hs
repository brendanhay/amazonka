{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleErrorCode
  ( LifecycleErrorCode
      ( LifecycleErrorCode',
        ScriptFailed,
        ScriptMissing,
        ScriptNotExecutable,
        ScriptTimedOut,
        Success,
        UnknownError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LifecycleErrorCode = LifecycleErrorCode' Lude.Text
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

pattern ScriptFailed :: LifecycleErrorCode
pattern ScriptFailed = LifecycleErrorCode' "ScriptFailed"

pattern ScriptMissing :: LifecycleErrorCode
pattern ScriptMissing = LifecycleErrorCode' "ScriptMissing"

pattern ScriptNotExecutable :: LifecycleErrorCode
pattern ScriptNotExecutable = LifecycleErrorCode' "ScriptNotExecutable"

pattern ScriptTimedOut :: LifecycleErrorCode
pattern ScriptTimedOut = LifecycleErrorCode' "ScriptTimedOut"

pattern Success :: LifecycleErrorCode
pattern Success = LifecycleErrorCode' "Success"

pattern UnknownError :: LifecycleErrorCode
pattern UnknownError = LifecycleErrorCode' "UnknownError"

{-# COMPLETE
  ScriptFailed,
  ScriptMissing,
  ScriptNotExecutable,
  ScriptTimedOut,
  Success,
  UnknownError,
  LifecycleErrorCode'
  #-}
