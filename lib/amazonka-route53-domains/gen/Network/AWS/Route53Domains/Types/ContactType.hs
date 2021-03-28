{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.ContactType
  ( ContactType
    ( ContactType'
    , ContactTypePerson
    , ContactTypeCompany
    , ContactTypeAssociation
    , ContactTypePublicBody
    , ContactTypeReseller
    , fromContactType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContactType = ContactType'{fromContactType :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ContactTypePerson :: ContactType
pattern ContactTypePerson = ContactType' "PERSON"

pattern ContactTypeCompany :: ContactType
pattern ContactTypeCompany = ContactType' "COMPANY"

pattern ContactTypeAssociation :: ContactType
pattern ContactTypeAssociation = ContactType' "ASSOCIATION"

pattern ContactTypePublicBody :: ContactType
pattern ContactTypePublicBody = ContactType' "PUBLIC_BODY"

pattern ContactTypeReseller :: ContactType
pattern ContactTypeReseller = ContactType' "RESELLER"

{-# COMPLETE 
  ContactTypePerson,

  ContactTypeCompany,

  ContactTypeAssociation,

  ContactTypePublicBody,

  ContactTypeReseller,
  ContactType'
  #-}
