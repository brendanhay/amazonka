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
-- Module      : Amazonka.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LifecycleErrorCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifecycleErrorCode = LifecycleErrorCode'
  { fromLifecycleErrorCode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
