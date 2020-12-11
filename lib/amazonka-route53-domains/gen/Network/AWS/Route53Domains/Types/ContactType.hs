-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactType
  ( ContactType
      ( ContactType',
        Association,
        Company,
        Person,
        PublicBody,
        Reseller
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContactType = ContactType' Lude.Text
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

pattern Association :: ContactType
pattern Association = ContactType' "ASSOCIATION"

pattern Company :: ContactType
pattern Company = ContactType' "COMPANY"

pattern Person :: ContactType
pattern Person = ContactType' "PERSON"

pattern PublicBody :: ContactType
pattern PublicBody = ContactType' "PUBLIC_BODY"

pattern Reseller :: ContactType
pattern Reseller = ContactType' "RESELLER"

{-# COMPLETE
  Association,
  Company,
  Person,
  PublicBody,
  Reseller,
  ContactType'
  #-}
