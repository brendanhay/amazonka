{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealthReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealthReason
  ( TargetHealthReason
      ( TargetHealthReason',
        AuthFailure,
        ConnectionFailed,
        PendingProxyCapacity,
        Unreachable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetHealthReason = TargetHealthReason' Lude.Text
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

pattern AuthFailure :: TargetHealthReason
pattern AuthFailure = TargetHealthReason' "AUTH_FAILURE"

pattern ConnectionFailed :: TargetHealthReason
pattern ConnectionFailed = TargetHealthReason' "CONNECTION_FAILED"

pattern PendingProxyCapacity :: TargetHealthReason
pattern PendingProxyCapacity = TargetHealthReason' "PENDING_PROXY_CAPACITY"

pattern Unreachable :: TargetHealthReason
pattern Unreachable = TargetHealthReason' "UNREACHABLE"

{-# COMPLETE
  AuthFailure,
  ConnectionFailed,
  PendingProxyCapacity,
  Unreachable,
  TargetHealthReason'
  #-}
