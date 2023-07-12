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
-- Module      : Amazonka.DirectoryService.Types.LDAPSStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.LDAPSStatus
  ( LDAPSStatus
      ( ..,
        LDAPSStatus_Disabled,
        LDAPSStatus_EnableFailed,
        LDAPSStatus_Enabled,
        LDAPSStatus_Enabling
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LDAPSStatus = LDAPSStatus'
  { fromLDAPSStatus ::
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

pattern LDAPSStatus_Disabled :: LDAPSStatus
pattern LDAPSStatus_Disabled = LDAPSStatus' "Disabled"

pattern LDAPSStatus_EnableFailed :: LDAPSStatus
pattern LDAPSStatus_EnableFailed = LDAPSStatus' "EnableFailed"

pattern LDAPSStatus_Enabled :: LDAPSStatus
pattern LDAPSStatus_Enabled = LDAPSStatus' "Enabled"

pattern LDAPSStatus_Enabling :: LDAPSStatus
pattern LDAPSStatus_Enabling = LDAPSStatus' "Enabling"

{-# COMPLETE
  LDAPSStatus_Disabled,
  LDAPSStatus_EnableFailed,
  LDAPSStatus_Enabled,
  LDAPSStatus_Enabling,
  LDAPSStatus'
  #-}
