{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.WriteForwardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.WriteForwardingStatus
  ( WriteForwardingStatus
      ( WriteForwardingStatus',
        Enabled,
        Disabled,
        Enabling,
        Disabling,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WriteForwardingStatus = WriteForwardingStatus' Lude.Text
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

pattern Enabled :: WriteForwardingStatus
pattern Enabled = WriteForwardingStatus' "enabled"

pattern Disabled :: WriteForwardingStatus
pattern Disabled = WriteForwardingStatus' "disabled"

pattern Enabling :: WriteForwardingStatus
pattern Enabling = WriteForwardingStatus' "enabling"

pattern Disabling :: WriteForwardingStatus
pattern Disabling = WriteForwardingStatus' "disabling"

pattern Unknown :: WriteForwardingStatus
pattern Unknown = WriteForwardingStatus' "unknown"

{-# COMPLETE
  Enabled,
  Disabled,
  Enabling,
  Disabling,
  Unknown,
  WriteForwardingStatus'
  #-}
