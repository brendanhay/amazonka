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
-- Module      : Amazonka.FSx.Types.LustreAccessAuditLogLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreAccessAuditLogLevel
  ( LustreAccessAuditLogLevel
      ( ..,
        LustreAccessAuditLogLevel_DISABLED,
        LustreAccessAuditLogLevel_ERROR_ONLY,
        LustreAccessAuditLogLevel_WARN_ERROR,
        LustreAccessAuditLogLevel_WARN_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LustreAccessAuditLogLevel = LustreAccessAuditLogLevel'
  { fromLustreAccessAuditLogLevel ::
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

pattern LustreAccessAuditLogLevel_DISABLED :: LustreAccessAuditLogLevel
pattern LustreAccessAuditLogLevel_DISABLED = LustreAccessAuditLogLevel' "DISABLED"

pattern LustreAccessAuditLogLevel_ERROR_ONLY :: LustreAccessAuditLogLevel
pattern LustreAccessAuditLogLevel_ERROR_ONLY = LustreAccessAuditLogLevel' "ERROR_ONLY"

pattern LustreAccessAuditLogLevel_WARN_ERROR :: LustreAccessAuditLogLevel
pattern LustreAccessAuditLogLevel_WARN_ERROR = LustreAccessAuditLogLevel' "WARN_ERROR"

pattern LustreAccessAuditLogLevel_WARN_ONLY :: LustreAccessAuditLogLevel
pattern LustreAccessAuditLogLevel_WARN_ONLY = LustreAccessAuditLogLevel' "WARN_ONLY"

{-# COMPLETE
  LustreAccessAuditLogLevel_DISABLED,
  LustreAccessAuditLogLevel_ERROR_ONLY,
  LustreAccessAuditLogLevel_WARN_ERROR,
  LustreAccessAuditLogLevel_WARN_ONLY,
  LustreAccessAuditLogLevel'
  #-}
