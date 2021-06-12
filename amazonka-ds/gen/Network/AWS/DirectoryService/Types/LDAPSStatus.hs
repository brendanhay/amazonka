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
-- Module      : Network.AWS.DirectoryService.Types.LDAPSStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSStatus
  ( LDAPSStatus
      ( ..,
        LDAPSStatus_Disabled,
        LDAPSStatus_EnableFailed,
        LDAPSStatus_Enabled,
        LDAPSStatus_Enabling
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LDAPSStatus = LDAPSStatus'
  { fromLDAPSStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
