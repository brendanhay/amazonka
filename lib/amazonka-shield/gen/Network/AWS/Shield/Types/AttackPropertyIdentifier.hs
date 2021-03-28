{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackPropertyIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackPropertyIdentifier
  ( AttackPropertyIdentifier
    ( AttackPropertyIdentifier'
    , AttackPropertyIdentifierDestinationUrl
    , AttackPropertyIdentifierReferrer
    , AttackPropertyIdentifierSourceAsn
    , AttackPropertyIdentifierSourceCountry
    , AttackPropertyIdentifierSourceIpAddress
    , AttackPropertyIdentifierSourceUserAgent
    , AttackPropertyIdentifierWordpressPingbackReflector
    , AttackPropertyIdentifierWordpressPingbackSource
    , fromAttackPropertyIdentifier
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AttackPropertyIdentifier = AttackPropertyIdentifier'{fromAttackPropertyIdentifier
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern AttackPropertyIdentifierDestinationUrl :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierDestinationUrl = AttackPropertyIdentifier' "DESTINATION_URL"

pattern AttackPropertyIdentifierReferrer :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierReferrer = AttackPropertyIdentifier' "REFERRER"

pattern AttackPropertyIdentifierSourceAsn :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierSourceAsn = AttackPropertyIdentifier' "SOURCE_ASN"

pattern AttackPropertyIdentifierSourceCountry :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierSourceCountry = AttackPropertyIdentifier' "SOURCE_COUNTRY"

pattern AttackPropertyIdentifierSourceIpAddress :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierSourceIpAddress = AttackPropertyIdentifier' "SOURCE_IP_ADDRESS"

pattern AttackPropertyIdentifierSourceUserAgent :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierSourceUserAgent = AttackPropertyIdentifier' "SOURCE_USER_AGENT"

pattern AttackPropertyIdentifierWordpressPingbackReflector :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierWordpressPingbackReflector = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_REFLECTOR"

pattern AttackPropertyIdentifierWordpressPingbackSource :: AttackPropertyIdentifier
pattern AttackPropertyIdentifierWordpressPingbackSource = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_SOURCE"

{-# COMPLETE 
  AttackPropertyIdentifierDestinationUrl,

  AttackPropertyIdentifierReferrer,

  AttackPropertyIdentifierSourceAsn,

  AttackPropertyIdentifierSourceCountry,

  AttackPropertyIdentifierSourceIpAddress,

  AttackPropertyIdentifierSourceUserAgent,

  AttackPropertyIdentifierWordpressPingbackReflector,

  AttackPropertyIdentifierWordpressPingbackSource,
  AttackPropertyIdentifier'
  #-}
