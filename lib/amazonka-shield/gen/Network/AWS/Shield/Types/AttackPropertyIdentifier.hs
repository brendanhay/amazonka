-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackPropertyIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackPropertyIdentifier
  ( AttackPropertyIdentifier
      ( AttackPropertyIdentifier',
        DestinationURL,
        Referrer,
        SourceASN,
        SourceCountry,
        SourceIPAddress,
        SourceUserAgent,
        WordpressPingbackReflector,
        WordpressPingbackSource
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AttackPropertyIdentifier = AttackPropertyIdentifier' Lude.Text
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

pattern DestinationURL :: AttackPropertyIdentifier
pattern DestinationURL = AttackPropertyIdentifier' "DESTINATION_URL"

pattern Referrer :: AttackPropertyIdentifier
pattern Referrer = AttackPropertyIdentifier' "REFERRER"

pattern SourceASN :: AttackPropertyIdentifier
pattern SourceASN = AttackPropertyIdentifier' "SOURCE_ASN"

pattern SourceCountry :: AttackPropertyIdentifier
pattern SourceCountry = AttackPropertyIdentifier' "SOURCE_COUNTRY"

pattern SourceIPAddress :: AttackPropertyIdentifier
pattern SourceIPAddress = AttackPropertyIdentifier' "SOURCE_IP_ADDRESS"

pattern SourceUserAgent :: AttackPropertyIdentifier
pattern SourceUserAgent = AttackPropertyIdentifier' "SOURCE_USER_AGENT"

pattern WordpressPingbackReflector :: AttackPropertyIdentifier
pattern WordpressPingbackReflector = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_REFLECTOR"

pattern WordpressPingbackSource :: AttackPropertyIdentifier
pattern WordpressPingbackSource = AttackPropertyIdentifier' "WORDPRESS_PINGBACK_SOURCE"

{-# COMPLETE
  DestinationURL,
  Referrer,
  SourceASN,
  SourceCountry,
  SourceIPAddress,
  SourceUserAgent,
  WordpressPingbackReflector,
  WordpressPingbackSource,
  AttackPropertyIdentifier'
  #-}
