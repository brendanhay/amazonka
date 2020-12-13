{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainAvailability
  ( DomainAvailability
      ( DomainAvailability',
        DAAvailable,
        DAAvailableReserved,
        DAAvailablePreorder,
        DAUnavailable,
        DAUnavailablePremium,
        DAUnavailableRestricted,
        DAReserved,
        DADontKnow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainAvailability = DomainAvailability' Lude.Text
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

pattern DAAvailable :: DomainAvailability
pattern DAAvailable = DomainAvailability' "AVAILABLE"

pattern DAAvailableReserved :: DomainAvailability
pattern DAAvailableReserved = DomainAvailability' "AVAILABLE_RESERVED"

pattern DAAvailablePreorder :: DomainAvailability
pattern DAAvailablePreorder = DomainAvailability' "AVAILABLE_PREORDER"

pattern DAUnavailable :: DomainAvailability
pattern DAUnavailable = DomainAvailability' "UNAVAILABLE"

pattern DAUnavailablePremium :: DomainAvailability
pattern DAUnavailablePremium = DomainAvailability' "UNAVAILABLE_PREMIUM"

pattern DAUnavailableRestricted :: DomainAvailability
pattern DAUnavailableRestricted = DomainAvailability' "UNAVAILABLE_RESTRICTED"

pattern DAReserved :: DomainAvailability
pattern DAReserved = DomainAvailability' "RESERVED"

pattern DADontKnow :: DomainAvailability
pattern DADontKnow = DomainAvailability' "DONT_KNOW"

{-# COMPLETE
  DAAvailable,
  DAAvailableReserved,
  DAAvailablePreorder,
  DAUnavailable,
  DAUnavailablePremium,
  DAUnavailableRestricted,
  DAReserved,
  DADontKnow,
  DomainAvailability'
  #-}
