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
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleErrorCode
  ( LifecycleErrorCode
      ( ..,
        LifecycleErrorCode_ScriptFailed,
        LifecycleErrorCode_ScriptMissing,
        LifecycleErrorCode_ScriptNotExecutable,
        LifecycleErrorCode_ScriptTimedOut,
        LifecycleErrorCode_Success,
        LifecycleErrorCode_UnknownError
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LifecycleErrorCode = LifecycleErrorCode'
  { fromLifecycleErrorCode ::
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

pattern LifecycleErrorCode_ScriptFailed :: LifecycleErrorCode
pattern LifecycleErrorCode_ScriptFailed = LifecycleErrorCode' "ScriptFailed"

pattern LifecycleErrorCode_ScriptMissing :: LifecycleErrorCode
pattern LifecycleErrorCode_ScriptMissing = LifecycleErrorCode' "ScriptMissing"

pattern LifecycleErrorCode_ScriptNotExecutable :: LifecycleErrorCode
pattern LifecycleErrorCode_ScriptNotExecutable = LifecycleErrorCode' "ScriptNotExecutable"

pattern LifecycleErrorCode_ScriptTimedOut :: LifecycleErrorCode
pattern LifecycleErrorCode_ScriptTimedOut = LifecycleErrorCode' "ScriptTimedOut"

pattern LifecycleErrorCode_Success :: LifecycleErrorCode
pattern LifecycleErrorCode_Success = LifecycleErrorCode' "Success"

pattern LifecycleErrorCode_UnknownError :: LifecycleErrorCode
pattern LifecycleErrorCode_UnknownError = LifecycleErrorCode' "UnknownError"

{-# COMPLETE
  LifecycleErrorCode_ScriptFailed,
  LifecycleErrorCode_ScriptMissing,
  LifecycleErrorCode_ScriptNotExecutable,
  LifecycleErrorCode_ScriptTimedOut,
  LifecycleErrorCode_Success,
  LifecycleErrorCode_UnknownError,
  LifecycleErrorCode'
  #-}
