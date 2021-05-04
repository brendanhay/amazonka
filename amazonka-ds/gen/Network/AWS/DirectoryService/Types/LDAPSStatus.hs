{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype LDAPSStatus = LDAPSStatus'
  { fromLDAPSStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
