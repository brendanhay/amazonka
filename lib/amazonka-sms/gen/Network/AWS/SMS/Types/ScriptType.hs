{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ScriptType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ScriptType where

import Network.AWS.Prelude

data ScriptType
  = PowershellScript
  | ShellScript
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

instance FromText ScriptType where
  parser =
    takeLowerText >>= \case
      "powershell_script" -> pure PowershellScript
      "shell_script" -> pure ShellScript
      e ->
        fromTextError $
          "Failure parsing ScriptType from value: '" <> e
            <> "'. Accepted values: powershell_script, shell_script"

instance ToText ScriptType where
  toText = \case
    PowershellScript -> "POWERSHELL_SCRIPT"
    ShellScript -> "SHELL_SCRIPT"

instance Hashable ScriptType

instance NFData ScriptType

instance ToByteString ScriptType

instance ToQuery ScriptType

instance ToHeader ScriptType

instance ToJSON ScriptType where
  toJSON = toJSONText

instance FromJSON ScriptType where
  parseJSON = parseJSONText "ScriptType"
