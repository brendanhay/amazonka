{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DnsNameState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DnsNameState
  ( DnsNameState
    ( DnsNameState'
    , DnsNameStatePendingVerification
    , DnsNameStateVerified
    , DnsNameStateFailed
    , fromDnsNameState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DnsNameState = DnsNameState'{fromDnsNameState :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern DnsNameStatePendingVerification :: DnsNameState
pattern DnsNameStatePendingVerification = DnsNameState' "pendingVerification"

pattern DnsNameStateVerified :: DnsNameState
pattern DnsNameStateVerified = DnsNameState' "verified"

pattern DnsNameStateFailed :: DnsNameState
pattern DnsNameStateFailed = DnsNameState' "failed"

{-# COMPLETE 
  DnsNameStatePendingVerification,

  DnsNameStateVerified,

  DnsNameStateFailed,
  DnsNameState'
  #-}
