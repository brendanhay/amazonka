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
    cdOrganizationName,
    cdEmail,
    cdState,
    cdFax,
    cdLastName,
    cdExtraParams,
    cdZipCode,
    cdAddressLine1,
    cdCity,
    cdPhoneNumber,
    cdAddressLine2,
    cdFirstName,
    cdCountryCode,
    cdContactType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.Types.ContactType
import Network.AWS.Route53Domains.Types.CountryCode
import Network.AWS.Route53Domains.Types.ExtraParam

-- | ContactDetail includes the following elements.
--
-- /See:/ 'mkContactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { -- | Name of the organization for contact types other than @PERSON@ .
    organizationName :: Lude.Maybe Lude.Text,
    -- | Email address of the contact.
    email :: Lude.Maybe Lude.Text,
    -- | The state or province of the contact's city.
    state :: Lude.Maybe Lude.Text,
    -- | Fax number of the contact.
    --
    -- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
    fax :: Lude.Maybe Lude.Text,
    -- | Last name of contact.
    lastName :: Lude.Maybe Lude.Text,
    -- | A list of name-value pairs for parameters required by certain top-level domains.
    extraParams :: Lude.Maybe [ExtraParam],
    -- | The zip or postal code of the contact's address.
    zipCode :: Lude.Maybe Lude.Text,
    -- | First line of the contact's address.
    addressLine1 :: Lude.Maybe Lude.Text,
    -- | The city of the contact's address.
    city :: Lude.Maybe Lude.Text,
    -- | The phone number of the contact.
    --
    -- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
    phoneNumber :: Lude.Maybe Lude.Text,
    -- | Second line of contact's address, if any.
    addressLine2 :: Lude.Maybe Lude.Text,
    -- | First name of contact.
    firstName :: Lude.Maybe Lude.Text,
    -- | Code for the country of the contact's address.
    countryCode :: Lude.Maybe CountryCode,
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
    contactType :: Lude.Maybe ContactType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContactDetail' with the minimum fields required to make a request.
--
-- * 'organizationName' - Name of the organization for contact types other than @PERSON@ .
-- * 'email' - Email address of the contact.
-- * 'state' - The state or province of the contact's city.
-- * 'fax' - Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
-- * 'lastName' - Last name of contact.
-- * 'extraParams' - A list of name-value pairs for parameters required by certain top-level domains.
-- * 'zipCode' - The zip or postal code of the contact's address.
-- * 'addressLine1' - First line of the contact's address.
-- * 'city' - The city of the contact's address.
-- * 'phoneNumber' - The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
-- * 'addressLine2' - Second line of contact's address, if any.
-- * 'firstName' - First name of contact.
-- * 'countryCode' - Code for the country of the contact's address.
-- * 'contactType' - Indicates whether the contact is a person, company, association, or public organization. Note the following:
--
--
--     * If you specify a value other than @PERSON@ , you must also specify a value for @OrganizationName@ .
--
--
--     * For some TLDs, the privacy protection available depends on the value that you specify for @Contact Type@ . For the privacy protection settings for your TLD, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/
--
--
--     * For .es domains, if you specify @PERSON@ , you must specify @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@ .
mkContactDetail ::
  ContactDetail
mkContactDetail =
  ContactDetail'
    { organizationName = Lude.Nothing,
      email = Lude.Nothing,
      state = Lude.Nothing,
      fax = Lude.Nothing,
      lastName = Lude.Nothing,
      extraParams = Lude.Nothing,
      zipCode = Lude.Nothing,
      addressLine1 = Lude.Nothing,
      city = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      addressLine2 = Lude.Nothing,
      firstName = Lude.Nothing,
      countryCode = Lude.Nothing,
      contactType = Lude.Nothing
    }

-- | Name of the organization for contact types other than @PERSON@ .
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOrganizationName :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdOrganizationName = Lens.lens (organizationName :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {organizationName = a} :: ContactDetail)
{-# DEPRECATED cdOrganizationName "Use generic-lens or generic-optics with 'organizationName' instead." #-}

-- | Email address of the contact.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEmail :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdEmail = Lens.lens (email :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: ContactDetail)
{-# DEPRECATED cdEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The state or province of the contact's city.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdState :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdState = Lens.lens (state :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ContactDetail)
{-# DEPRECATED cdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- /Note:/ Consider using 'fax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFax :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdFax = Lens.lens (fax :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {fax = a} :: ContactDetail)
{-# DEPRECATED cdFax "Use generic-lens or generic-optics with 'fax' instead." #-}

-- | Last name of contact.
--
-- /Note:/ Consider using 'lastName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastName :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdLastName = Lens.lens (lastName :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {lastName = a} :: ContactDetail)
{-# DEPRECATED cdLastName "Use generic-lens or generic-optics with 'lastName' instead." #-}

-- | A list of name-value pairs for parameters required by certain top-level domains.
--
-- /Note:/ Consider using 'extraParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExtraParams :: Lens.Lens' ContactDetail (Lude.Maybe [ExtraParam])
cdExtraParams = Lens.lens (extraParams :: ContactDetail -> Lude.Maybe [ExtraParam]) (\s a -> s {extraParams = a} :: ContactDetail)
{-# DEPRECATED cdExtraParams "Use generic-lens or generic-optics with 'extraParams' instead." #-}

-- | The zip or postal code of the contact's address.
--
-- /Note:/ Consider using 'zipCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdZipCode :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdZipCode = Lens.lens (zipCode :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {zipCode = a} :: ContactDetail)
{-# DEPRECATED cdZipCode "Use generic-lens or generic-optics with 'zipCode' instead." #-}

-- | First line of the contact's address.
--
-- /Note:/ Consider using 'addressLine1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAddressLine1 :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdAddressLine1 = Lens.lens (addressLine1 :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {addressLine1 = a} :: ContactDetail)
{-# DEPRECATED cdAddressLine1 "Use generic-lens or generic-optics with 'addressLine1' instead." #-}

-- | The city of the contact's address.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCity :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdCity = Lens.lens (city :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: ContactDetail)
{-# DEPRECATED cdCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPhoneNumber :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdPhoneNumber = Lens.lens (phoneNumber :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: ContactDetail)
{-# DEPRECATED cdPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | Second line of contact's address, if any.
--
-- /Note:/ Consider using 'addressLine2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAddressLine2 :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdAddressLine2 = Lens.lens (addressLine2 :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {addressLine2 = a} :: ContactDetail)
{-# DEPRECATED cdAddressLine2 "Use generic-lens or generic-optics with 'addressLine2' instead." #-}

-- | First name of contact.
--
-- /Note:/ Consider using 'firstName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFirstName :: Lens.Lens' ContactDetail (Lude.Maybe Lude.Text)
cdFirstName = Lens.lens (firstName :: ContactDetail -> Lude.Maybe Lude.Text) (\s a -> s {firstName = a} :: ContactDetail)
{-# DEPRECATED cdFirstName "Use generic-lens or generic-optics with 'firstName' instead." #-}

-- | Code for the country of the contact's address.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCountryCode :: Lens.Lens' ContactDetail (Lude.Maybe CountryCode)
cdCountryCode = Lens.lens (countryCode :: ContactDetail -> Lude.Maybe CountryCode) (\s a -> s {countryCode = a} :: ContactDetail)
{-# DEPRECATED cdCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

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
cdContactType :: Lens.Lens' ContactDetail (Lude.Maybe ContactType)
cdContactType = Lens.lens (contactType :: ContactDetail -> Lude.Maybe ContactType) (\s a -> s {contactType = a} :: ContactDetail)
{-# DEPRECATED cdContactType "Use generic-lens or generic-optics with 'contactType' instead." #-}

instance Lude.FromJSON ContactDetail where
  parseJSON =
    Lude.withObject
      "ContactDetail"
      ( \x ->
          ContactDetail'
            Lude.<$> (x Lude..:? "OrganizationName")
            Lude.<*> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Fax")
            Lude.<*> (x Lude..:? "LastName")
            Lude.<*> (x Lude..:? "ExtraParams" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ZipCode")
            Lude.<*> (x Lude..:? "AddressLine1")
            Lude.<*> (x Lude..:? "City")
            Lude.<*> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..:? "AddressLine2")
            Lude.<*> (x Lude..:? "FirstName")
            Lude.<*> (x Lude..:? "CountryCode")
            Lude.<*> (x Lude..:? "ContactType")
      )

instance Lude.ToJSON ContactDetail where
  toJSON ContactDetail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationName" Lude..=) Lude.<$> organizationName,
            ("Email" Lude..=) Lude.<$> email,
            ("State" Lude..=) Lude.<$> state,
            ("Fax" Lude..=) Lude.<$> fax,
            ("LastName" Lude..=) Lude.<$> lastName,
            ("ExtraParams" Lude..=) Lude.<$> extraParams,
            ("ZipCode" Lude..=) Lude.<$> zipCode,
            ("AddressLine1" Lude..=) Lude.<$> addressLine1,
            ("City" Lude..=) Lude.<$> city,
            ("PhoneNumber" Lude..=) Lude.<$> phoneNumber,
            ("AddressLine2" Lude..=) Lude.<$> addressLine2,
            ("FirstName" Lude..=) Lude.<$> firstName,
            ("CountryCode" Lude..=) Lude.<$> countryCode,
            ("ContactType" Lude..=) Lude.<$> contactType
          ]
      )
