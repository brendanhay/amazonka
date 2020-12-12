{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSStatus
  ( LDAPSStatus
      ( LDAPSStatus',
        LDAPSSDisabled,
        LDAPSSEnableFailed,
        LDAPSSEnabled,
        LDAPSSEnabling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LDAPSStatus = LDAPSStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern LDAPSSDisabled :: LDAPSStatus
pattern LDAPSSDisabled = LDAPSStatus' "Disabled"

pattern LDAPSSEnableFailed :: LDAPSStatus
pattern LDAPSSEnableFailed = LDAPSStatus' "EnableFailed"

pattern LDAPSSEnabled :: LDAPSStatus
pattern LDAPSSEnabled = LDAPSStatus' "Enabled"

pattern LDAPSSEnabling :: LDAPSStatus
pattern LDAPSSEnabling = LDAPSStatus' "Enabling"

{-# COMPLETE
  LDAPSSDisabled,
  LDAPSSEnableFailed,
  LDAPSSEnabled,
  LDAPSSEnabling,
  LDAPSStatus'
  #-}
