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
-- Module      : Amazonka.SMS.Types.ScriptType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ScriptType
  ( ScriptType
      ( ..,
        ScriptType_POWERSHELL_SCRIPT,
        ScriptType_SHELL_SCRIPT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ScriptType = ScriptType'
  { fromScriptType ::
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

pattern ScriptType_POWERSHELL_SCRIPT :: ScriptType
pattern ScriptType_POWERSHELL_SCRIPT = ScriptType' "POWERSHELL_SCRIPT"

pattern ScriptType_SHELL_SCRIPT :: ScriptType
pattern ScriptType_SHELL_SCRIPT = ScriptType' "SHELL_SCRIPT"

{-# COMPLETE
  ScriptType_POWERSHELL_SCRIPT,
  ScriptType_SHELL_SCRIPT,
  ScriptType'
  #-}
