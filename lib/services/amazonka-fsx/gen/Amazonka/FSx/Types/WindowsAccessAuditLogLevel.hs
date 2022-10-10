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
-- Module      : Amazonka.FSx.Types.WindowsAccessAuditLogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.WindowsAccessAuditLogLevel
  ( WindowsAccessAuditLogLevel
      ( ..,
        WindowsAccessAuditLogLevel_DISABLED,
        WindowsAccessAuditLogLevel_FAILURE_ONLY,
        WindowsAccessAuditLogLevel_SUCCESS_AND_FAILURE,
        WindowsAccessAuditLogLevel_SUCCESS_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WindowsAccessAuditLogLevel = WindowsAccessAuditLogLevel'
  { fromWindowsAccessAuditLogLevel ::
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

pattern WindowsAccessAuditLogLevel_DISABLED :: WindowsAccessAuditLogLevel
pattern WindowsAccessAuditLogLevel_DISABLED = WindowsAccessAuditLogLevel' "DISABLED"

pattern WindowsAccessAuditLogLevel_FAILURE_ONLY :: WindowsAccessAuditLogLevel
pattern WindowsAccessAuditLogLevel_FAILURE_ONLY = WindowsAccessAuditLogLevel' "FAILURE_ONLY"

pattern WindowsAccessAuditLogLevel_SUCCESS_AND_FAILURE :: WindowsAccessAuditLogLevel
pattern WindowsAccessAuditLogLevel_SUCCESS_AND_FAILURE = WindowsAccessAuditLogLevel' "SUCCESS_AND_FAILURE"

pattern WindowsAccessAuditLogLevel_SUCCESS_ONLY :: WindowsAccessAuditLogLevel
pattern WindowsAccessAuditLogLevel_SUCCESS_ONLY = WindowsAccessAuditLogLevel' "SUCCESS_ONLY"

{-# COMPLETE
  WindowsAccessAuditLogLevel_DISABLED,
  WindowsAccessAuditLogLevel_FAILURE_ONLY,
  WindowsAccessAuditLogLevel_SUCCESS_AND_FAILURE,
  WindowsAccessAuditLogLevel_SUCCESS_ONLY,
  WindowsAccessAuditLogLevel'
  #-}
