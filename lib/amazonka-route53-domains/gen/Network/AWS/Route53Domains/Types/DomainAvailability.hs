{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.DomainAvailability
  ( DomainAvailability
    ( DomainAvailability'
    , DomainAvailabilityAvailable
    , DomainAvailabilityAvailableReserved
    , DomainAvailabilityAvailablePreorder
    , DomainAvailabilityUnavailable
    , DomainAvailabilityUnavailablePremium
    , DomainAvailabilityUnavailableRestricted
    , DomainAvailabilityReserved
    , DomainAvailabilityDontKnow
    , fromDomainAvailability
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DomainAvailability = DomainAvailability'{fromDomainAvailability
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern DomainAvailabilityAvailable :: DomainAvailability
pattern DomainAvailabilityAvailable = DomainAvailability' "AVAILABLE"

pattern DomainAvailabilityAvailableReserved :: DomainAvailability
pattern DomainAvailabilityAvailableReserved = DomainAvailability' "AVAILABLE_RESERVED"

pattern DomainAvailabilityAvailablePreorder :: DomainAvailability
pattern DomainAvailabilityAvailablePreorder = DomainAvailability' "AVAILABLE_PREORDER"

pattern DomainAvailabilityUnavailable :: DomainAvailability
pattern DomainAvailabilityUnavailable = DomainAvailability' "UNAVAILABLE"

pattern DomainAvailabilityUnavailablePremium :: DomainAvailability
pattern DomainAvailabilityUnavailablePremium = DomainAvailability' "UNAVAILABLE_PREMIUM"

pattern DomainAvailabilityUnavailableRestricted :: DomainAvailability
pattern DomainAvailabilityUnavailableRestricted = DomainAvailability' "UNAVAILABLE_RESTRICTED"

pattern DomainAvailabilityReserved :: DomainAvailability
pattern DomainAvailabilityReserved = DomainAvailability' "RESERVED"

pattern DomainAvailabilityDontKnow :: DomainAvailability
pattern DomainAvailabilityDontKnow = DomainAvailability' "DONT_KNOW"

{-# COMPLETE 
  DomainAvailabilityAvailable,

  DomainAvailabilityAvailableReserved,

  DomainAvailabilityAvailablePreorder,

  DomainAvailabilityUnavailable,

  DomainAvailabilityUnavailablePremium,

  DomainAvailabilityUnavailableRestricted,

  DomainAvailabilityReserved,

  DomainAvailabilityDontKnow,
  DomainAvailability'
  #-}
