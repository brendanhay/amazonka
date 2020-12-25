{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactDetail
  ( ContactDetail (..),

    -- * Smart constructor
    mkContactDetail,

    -- * Lenses
    cdAddressLine1,
    cdAddressLine2,
    cdCity,
    cdContactType,
    cdCountryCode,
    cdEmail,
    cdExtraParams,
    cdFax,
    cdFirstName,
    cdLastName,
    cdOrganizationName,
    cdPhoneNumber,
    cdState,
    cdZipCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.AddressLine as Types
import qualified Network.AWS.Route53Domains.Types.City as Types
import qualified Network.AWS.Route53Domains.Types.ContactName as Types
import qualified Network.AWS.Route53Domains.Types.ContactNumber as Types
import qualified Network.AWS.Route53Domains.Types.ContactType as Types
import qualified Network.AWS.Route53Domains.Types.CountryCode as Types
import qualified Network.AWS.Route53Domains.Types.Email as Types
import qualified Network.AWS.Route53Domains.Types.ExtraParam as Types
import qualified Network.AWS.Route53Domains.Types.State as Types
import qualified Network.AWS.Route53Domains.Types.ZipCode as Types

-- | ContactDetail includes the following elements.
--
-- /See:/ 'mkContactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { -- | First line of the contact's address.
    addressLine1 :: Core.Maybe Types.AddressLine,
    -- | Second line of contact's address, if any.
    addressLine2 :: Core.Maybe Types.AddressLine,
    -- | The city of the contact's address.
    city :: Core.Maybe Types.City,
    -- | Indicates whether the contact is a person, company, association, or public organization. Note the following:
    --
    --
    --     * If you specify a value other than @PERSON@ , you must also specify a value for @OrganizationName@ .
    --
    --
    --     * For some TLDs, the privacy protection available depends on the value that you specify for @Contact Type@ . For the privacy protection settings for your TLD, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/
    --
    --
    --     * For .es domains, if you specify @PERSON@ , you must specify @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@ .
    contactType :: Core.Maybe Types.ContactType,
    -- | Code for the country of the contact's address.
    countryCode :: Core.Maybe Types.CountryCode,
    -- | Email address of the contact.
    email :: Core.Maybe Types.Email,
    -- | A list of name-value pairs for parameters required by certain top-level domains.
    extraParams :: Core.Maybe [Types.ExtraParam],
    -- | Fax number of the contact.
    --
    -- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
    fax :: Core.Maybe Types.ContactNumber,
    -- | First name of contact.
    firstName :: Core.Maybe Types.ContactName,
    -- | Last name of contact.
    lastName :: Core.Maybe Types.ContactName,
    -- | Name of the organization for contact types other than @PERSON@ .
    organizationName :: Core.Maybe Types.ContactName,
    -- | The phone number of the contact.
    --
    -- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
    phoneNumber :: Core.Maybe Types.ContactNumber,
    -- | The state or province of the contact's city.
    state :: Core.Maybe Types.State,
    -- | The zip or postal code of the contact's address.
    zipCode :: Core.Maybe Types.ZipCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContactDetail' value with any optional fields omitted.
mkContactDetail ::
  ContactDetail
mkContactDetail =
  ContactDetail'
    { addressLine1 = Core.Nothing,
      addressLine2 = Core.Nothing,
      city = Core.Nothing,
      contactType = Core.Nothing,
      countryCode = Core.Nothing,
      email = Core.Nothing,
      extraParams = Core.Nothing,
      fax = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      organizationName = Core.Nothing,
      phoneNumber = Core.Nothing,
      state = Core.Nothing,
      zipCode = Core.Nothing
    }

-- | First line of the contact's address.
--
-- /Note:/ Consider using 'addressLine1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAddressLine1 :: Lens.Lens' ContactDetail (Core.Maybe Types.AddressLine)
cdAddressLine1 = Lens.field @"addressLine1"
{-# DEPRECATED cdAddressLine1 "Use generic-lens or generic-optics with 'addressLine1' instead." #-}

-- | Second line of contact's address, if any.
--
-- /Note:/ Consider using 'addressLine2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAddressLine2 :: Lens.Lens' ContactDetail (Core.Maybe Types.AddressLine)
cdAddressLine2 = Lens.field @"addressLine2"
{-# DEPRECATED cdAddressLine2 "Use generic-lens or generic-optics with 'addressLine2' instead." #-}

-- | The city of the contact's address.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCity :: Lens.Lens' ContactDetail (Core.Maybe Types.City)
cdCity = Lens.field @"city"
{-# DEPRECATED cdCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | Indicates whether the contact is a person, company, association, or public organization. Note the following:
--
--
--     * If you specify a value other than @PERSON@ , you must also specify a value for @OrganizationName@ .
--
--
--     * For some TLDs, the privacy protection available depends on the value that you specify for @Contact Type@ . For the privacy protection settings for your TLD, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/
--
--
--     * For .es domains, if you specify @PERSON@ , you must specify @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@ .
--
--
--
-- /Note:/ Consider using 'contactType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdContactType :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactType)
cdContactType = Lens.field @"contactType"
{-# DEPRECATED cdContactType "Use generic-lens or generic-optics with 'contactType' instead." #-}

-- | Code for the country of the contact's address.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCountryCode :: Lens.Lens' ContactDetail (Core.Maybe Types.CountryCode)
cdCountryCode = Lens.field @"countryCode"
{-# DEPRECATED cdCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | Email address of the contact.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEmail :: Lens.Lens' ContactDetail (Core.Maybe Types.Email)
cdEmail = Lens.field @"email"
{-# DEPRECATED cdEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | A list of name-value pairs for parameters required by certain top-level domains.
--
-- /Note:/ Consider using 'extraParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExtraParams :: Lens.Lens' ContactDetail (Core.Maybe [Types.ExtraParam])
cdExtraParams = Lens.field @"extraParams"
{-# DEPRECATED cdExtraParams "Use generic-lens or generic-optics with 'extraParams' instead." #-}

-- | Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- /Note:/ Consider using 'fax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFax :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactNumber)
cdFax = Lens.field @"fax"
{-# DEPRECATED cdFax "Use generic-lens or generic-optics with 'fax' instead." #-}

-- | First name of contact.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFirstName :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactName)
cdFirstName = Lens.field @"firstName"
{-# DEPRECATED cdFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | Last name of contact.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastName :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactName)
cdLastName = Lens.field @"lastName"
{-# DEPRECATED cdLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | Name of the organization for contact types other than @PERSON@ .
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOrganizationName :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactName)
cdOrganizationName = Lens.field @"organizationName"
{-# DEPRECATED cdOrganizationName "Use generic-lens or generic-optics with 'organizationName' instead." #-}

-- | The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumber :: Lens.Lens' ContactDetail (Core.Maybe Types.ContactNumber)
cdPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED cdPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The state or province of the contact's city.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdState :: Lens.Lens' ContactDetail (Core.Maybe Types.State)
cdState = Lens.field @"state"
{-# DEPRECATED cdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The zip or postal code of the contact's address.
--
-- /Note:/ Consider using 'zipCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdZipCode :: Lens.Lens' ContactDetail (Core.Maybe Types.ZipCode)
cdZipCode = Lens.field @"zipCode"
{-# DEPRECATED cdZipCode "Use generic-lens or generic-optics with 'zipCode' instead." #-}

instance Core.FromJSON ContactDetail where
  toJSON ContactDetail {..} =
    Core.object
      ( Core.catMaybes
          [ ("AddressLine1" Core..=) Core.<$> addressLine1,
            ("AddressLine2" Core..=) Core.<$> addressLine2,
            ("City" Core..=) Core.<$> city,
            ("ContactType" Core..=) Core.<$> contactType,
            ("CountryCode" Core..=) Core.<$> countryCode,
            ("Email" Core..=) Core.<$> email,
            ("ExtraParams" Core..=) Core.<$> extraParams,
            ("Fax" Core..=) Core.<$> fax,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            ("OrganizationName" Core..=) Core.<$> organizationName,
            ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("State" Core..=) Core.<$> state,
            ("ZipCode" Core..=) Core.<$> zipCode
          ]
      )

instance Core.FromJSON ContactDetail where
  parseJSON =
    Core.withObject "ContactDetail" Core.$
      \x ->
        ContactDetail'
          Core.<$> (x Core..:? "AddressLine1")
          Core.<*> (x Core..:? "AddressLine2")
          Core.<*> (x Core..:? "City")
          Core.<*> (x Core..:? "ContactType")
          Core.<*> (x Core..:? "CountryCode")
          Core.<*> (x Core..:? "Email")
          Core.<*> (x Core..:? "ExtraParams")
          Core.<*> (x Core..:? "Fax")
          Core.<*> (x Core..:? "FirstName")
          Core.<*> (x Core..:? "LastName")
          Core.<*> (x Core..:? "OrganizationName")
          Core.<*> (x Core..:? "PhoneNumber")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "ZipCode")
