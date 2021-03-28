{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealthReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.TargetHealthReason
  ( TargetHealthReason
    ( TargetHealthReason'
    , TargetHealthReasonUnreachable
    , TargetHealthReasonConnectionFailed
    , TargetHealthReasonAuthFailure
    , TargetHealthReasonPendingProxyCapacity
    , fromTargetHealthReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TargetHealthReason = TargetHealthReason'{fromTargetHealthReason
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern TargetHealthReasonUnreachable :: TargetHealthReason
pattern TargetHealthReasonUnreachable = TargetHealthReason' "UNREACHABLE"

pattern TargetHealthReasonConnectionFailed :: TargetHealthReason
pattern TargetHealthReasonConnectionFailed = TargetHealthReason' "CONNECTION_FAILED"

pattern TargetHealthReasonAuthFailure :: TargetHealthReason
pattern TargetHealthReasonAuthFailure = TargetHealthReason' "AUTH_FAILURE"

pattern TargetHealthReasonPendingProxyCapacity :: TargetHealthReason
pattern TargetHealthReasonPendingProxyCapacity = TargetHealthReason' "PENDING_PROXY_CAPACITY"

{-# COMPLETE 
  TargetHealthReasonUnreachable,

  TargetHealthReasonConnectionFailed,

  TargetHealthReasonAuthFailure,

  TargetHealthReasonPendingProxyCapacity,
  TargetHealthReason'
  #-}
