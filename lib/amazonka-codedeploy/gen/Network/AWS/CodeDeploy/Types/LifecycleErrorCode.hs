{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleErrorCode where

import Network.AWS.Prelude

data LifecycleErrorCode
  = ScriptFailed
  | ScriptMissing
  | ScriptNotExecutable
  | ScriptTimedOut
  | Success
  | UnknownError
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText LifecycleErrorCode where
  parser =
    takeLowerText >>= \case
      "scriptfailed" -> pure ScriptFailed
      "scriptmissing" -> pure ScriptMissing
      "scriptnotexecutable" -> pure ScriptNotExecutable
      "scripttimedout" -> pure ScriptTimedOut
      "success" -> pure Success
      "unknownerror" -> pure UnknownError
      e ->
        fromTextError $
          "Failure parsing LifecycleErrorCode from value: '" <> e
            <> "'. Accepted values: scriptfailed, scriptmissing, scriptnotexecutable, scripttimedout, success, unknownerror"

instance ToText LifecycleErrorCode where
  toText = \case
    ScriptFailed -> "ScriptFailed"
    ScriptMissing -> "ScriptMissing"
    ScriptNotExecutable -> "ScriptNotExecutable"
    ScriptTimedOut -> "ScriptTimedOut"
    Success -> "Success"
    UnknownError -> "UnknownError"

instance Hashable LifecycleErrorCode

instance NFData LifecycleErrorCode

instance ToByteString LifecycleErrorCode

instance ToQuery LifecycleErrorCode

instance ToHeader LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
  parseJSON = parseJSONText "LifecycleErrorCode"
