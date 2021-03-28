{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.LifecycleErrorCode
  ( LifecycleErrorCode
    ( LifecycleErrorCode'
    , LifecycleErrorCodeSuccess
    , LifecycleErrorCodeScriptMissing
    , LifecycleErrorCodeScriptNotExecutable
    , LifecycleErrorCodeScriptTimedOut
    , LifecycleErrorCodeScriptFailed
    , LifecycleErrorCodeUnknownError
    , fromLifecycleErrorCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LifecycleErrorCode = LifecycleErrorCode'{fromLifecycleErrorCode
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern LifecycleErrorCodeSuccess :: LifecycleErrorCode
pattern LifecycleErrorCodeSuccess = LifecycleErrorCode' "Success"

pattern LifecycleErrorCodeScriptMissing :: LifecycleErrorCode
pattern LifecycleErrorCodeScriptMissing = LifecycleErrorCode' "ScriptMissing"

pattern LifecycleErrorCodeScriptNotExecutable :: LifecycleErrorCode
pattern LifecycleErrorCodeScriptNotExecutable = LifecycleErrorCode' "ScriptNotExecutable"

pattern LifecycleErrorCodeScriptTimedOut :: LifecycleErrorCode
pattern LifecycleErrorCodeScriptTimedOut = LifecycleErrorCode' "ScriptTimedOut"

pattern LifecycleErrorCodeScriptFailed :: LifecycleErrorCode
pattern LifecycleErrorCodeScriptFailed = LifecycleErrorCode' "ScriptFailed"

pattern LifecycleErrorCodeUnknownError :: LifecycleErrorCode
pattern LifecycleErrorCodeUnknownError = LifecycleErrorCode' "UnknownError"

{-# COMPLETE 
  LifecycleErrorCodeSuccess,

  LifecycleErrorCodeScriptMissing,

  LifecycleErrorCodeScriptNotExecutable,

  LifecycleErrorCodeScriptTimedOut,

  LifecycleErrorCodeScriptFailed,

  LifecycleErrorCodeUnknownError,
  LifecycleErrorCode'
  #-}
