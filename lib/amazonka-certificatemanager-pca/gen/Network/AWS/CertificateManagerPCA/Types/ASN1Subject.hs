{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ASN1Subject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ASN1Subject
  ( ASN1Subject (..),

    -- * Smart constructor
    mkASN1Subject,

    -- * Lenses
    asGivenName,
    asState,
    asCommonName,
    asOrganizationalUnit,
    asCountry,
    asGenerationQualifier,
    asLocality,
    asPseudonym,
    asInitials,
    asTitle,
    asOrganization,
    asSerialNumber,
    asSurname,
    asDistinguishedNameQualifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the certificate subject. The certificate can be one issued by your private certificate authority (CA) or it can be your private CA certificate. The __Subject__ field in the certificate identifies the entity that owns or controls the public key in the certificate. The entity can be a user, computer, device, or service. The __Subject__ must contain an X.500 distinguished name (DN). A DN is a sequence of relative distinguished names (RDNs). The RDNs are separated by commas in the certificate. The DN must be unique for each entity, but your private CA can issue more than one certificate with the same DN to the same entity.
--
-- /See:/ 'mkASN1Subject' smart constructor.
data ASN1Subject = ASN1Subject'
  { -- | First name.
    givenName :: Lude.Maybe Lude.Text,
    -- | State in which the subject of the certificate is located.
    state :: Lude.Maybe Lude.Text,
    -- | Fully qualified domain name (FQDN) associated with the certificate subject.
    commonName :: Lude.Maybe Lude.Text,
    -- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
    organizationalUnit :: Lude.Maybe Lude.Text,
    -- | Two-digit code that specifies the country in which the certificate subject located.
    country :: Lude.Maybe Lude.Text,
    -- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
    generationQualifier :: Lude.Maybe Lude.Text,
    -- | The locality (such as a city or town) in which the certificate subject is located.
    locality :: Lude.Maybe Lude.Text,
    -- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
    pseudonym :: Lude.Maybe Lude.Text,
    -- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
    initials :: Lude.Maybe Lude.Text,
    -- | A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
    title :: Lude.Maybe Lude.Text,
    -- | Legal name of the organization with which the certificate subject is affiliated.
    organization :: Lude.Maybe Lude.Text,
    -- | The certificate serial number.
    serialNumber :: Lude.Maybe Lude.Text,
    -- | Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
    surname :: Lude.Maybe Lude.Text,
    -- | Disambiguating information for the certificate subject.
    distinguishedNameQualifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ASN1Subject' with the minimum fields required to make a request.
--
-- * 'givenName' - First name.
-- * 'state' - State in which the subject of the certificate is located.
-- * 'commonName' - Fully qualified domain name (FQDN) associated with the certificate subject.
-- * 'organizationalUnit' - A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
-- * 'country' - Two-digit code that specifies the country in which the certificate subject located.
-- * 'generationQualifier' - Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
-- * 'locality' - The locality (such as a city or town) in which the certificate subject is located.
-- * 'pseudonym' - Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
-- * 'initials' - Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
-- * 'title' - A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
-- * 'organization' - Legal name of the organization with which the certificate subject is affiliated.
-- * 'serialNumber' - The certificate serial number.
-- * 'surname' - Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
-- * 'distinguishedNameQualifier' - Disambiguating information for the certificate subject.
mkASN1Subject ::
  ASN1Subject
mkASN1Subject =
  ASN1Subject'
    { givenName = Lude.Nothing,
      state = Lude.Nothing,
      commonName = Lude.Nothing,
      organizationalUnit = Lude.Nothing,
      country = Lude.Nothing,
      generationQualifier = Lude.Nothing,
      locality = Lude.Nothing,
      pseudonym = Lude.Nothing,
      initials = Lude.Nothing,
      title = Lude.Nothing,
      organization = Lude.Nothing,
      serialNumber = Lude.Nothing,
      surname = Lude.Nothing,
      distinguishedNameQualifier = Lude.Nothing
    }

-- | First name.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asGivenName :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asGivenName = Lens.lens (givenName :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {givenName = a} :: ASN1Subject)
{-# DEPRECATED asGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | State in which the subject of the certificate is located.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asState :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asState = Lens.lens (state :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ASN1Subject)
{-# DEPRECATED asState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Fully qualified domain name (FQDN) associated with the certificate subject.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCommonName :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asCommonName = Lens.lens (commonName :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {commonName = a} :: ASN1Subject)
{-# DEPRECATED asCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

-- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asOrganizationalUnit :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asOrganizationalUnit = Lens.lens (organizationalUnit :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnit = a} :: ASN1Subject)
{-# DEPRECATED asOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | Two-digit code that specifies the country in which the certificate subject located.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCountry :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asCountry = Lens.lens (country :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: ASN1Subject)
{-# DEPRECATED asCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
--
-- /Note:/ Consider using 'generationQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asGenerationQualifier :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asGenerationQualifier = Lens.lens (generationQualifier :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {generationQualifier = a} :: ASN1Subject)
{-# DEPRECATED asGenerationQualifier "Use generic-lens or generic-optics with 'generationQualifier' instead." #-}

-- | The locality (such as a city or town) in which the certificate subject is located.
--
-- /Note:/ Consider using 'locality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLocality :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asLocality = Lens.lens (locality :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {locality = a} :: ASN1Subject)
{-# DEPRECATED asLocality "Use generic-lens or generic-optics with 'locality' instead." #-}

-- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
--
-- /Note:/ Consider using 'pseudonym' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asPseudonym :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asPseudonym = Lens.lens (pseudonym :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {pseudonym = a} :: ASN1Subject)
{-# DEPRECATED asPseudonym "Use generic-lens or generic-optics with 'pseudonym' instead." #-}

-- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
--
-- /Note:/ Consider using 'initials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asInitials :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asInitials = Lens.lens (initials :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {initials = a} :: ASN1Subject)
{-# DEPRECATED asInitials "Use generic-lens or generic-optics with 'initials' instead." #-}

-- | A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTitle :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asTitle = Lens.lens (title :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: ASN1Subject)
{-# DEPRECATED asTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Legal name of the organization with which the certificate subject is affiliated.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asOrganization :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asOrganization = Lens.lens (organization :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {organization = a} :: ASN1Subject)
{-# DEPRECATED asOrganization "Use generic-lens or generic-optics with 'organization' instead." #-}

-- | The certificate serial number.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSerialNumber :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asSerialNumber = Lens.lens (serialNumber :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: ASN1Subject)
{-# DEPRECATED asSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSurname :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asSurname = Lens.lens (surname :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {surname = a} :: ASN1Subject)
{-# DEPRECATED asSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | Disambiguating information for the certificate subject.
--
-- /Note:/ Consider using 'distinguishedNameQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDistinguishedNameQualifier :: Lens.Lens' ASN1Subject (Lude.Maybe Lude.Text)
asDistinguishedNameQualifier = Lens.lens (distinguishedNameQualifier :: ASN1Subject -> Lude.Maybe Lude.Text) (\s a -> s {distinguishedNameQualifier = a} :: ASN1Subject)
{-# DEPRECATED asDistinguishedNameQualifier "Use generic-lens or generic-optics with 'distinguishedNameQualifier' instead." #-}

instance Lude.FromJSON ASN1Subject where
  parseJSON =
    Lude.withObject
      "ASN1Subject"
      ( \x ->
          ASN1Subject'
            Lude.<$> (x Lude..:? "GivenName")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CommonName")
            Lude.<*> (x Lude..:? "OrganizationalUnit")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "GenerationQualifier")
            Lude.<*> (x Lude..:? "Locality")
            Lude.<*> (x Lude..:? "Pseudonym")
            Lude.<*> (x Lude..:? "Initials")
            Lude.<*> (x Lude..:? "Title")
            Lude.<*> (x Lude..:? "Organization")
            Lude.<*> (x Lude..:? "SerialNumber")
            Lude.<*> (x Lude..:? "Surname")
            Lude.<*> (x Lude..:? "DistinguishedNameQualifier")
      )

instance Lude.ToJSON ASN1Subject where
  toJSON ASN1Subject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GivenName" Lude..=) Lude.<$> givenName,
            ("State" Lude..=) Lude.<$> state,
            ("CommonName" Lude..=) Lude.<$> commonName,
            ("OrganizationalUnit" Lude..=) Lude.<$> organizationalUnit,
            ("Country" Lude..=) Lude.<$> country,
            ("GenerationQualifier" Lude..=) Lude.<$> generationQualifier,
            ("Locality" Lude..=) Lude.<$> locality,
            ("Pseudonym" Lude..=) Lude.<$> pseudonym,
            ("Initials" Lude..=) Lude.<$> initials,
            ("Title" Lude..=) Lude.<$> title,
            ("Organization" Lude..=) Lude.<$> organization,
            ("SerialNumber" Lude..=) Lude.<$> serialNumber,
            ("Surname" Lude..=) Lude.<$> surname,
            ("DistinguishedNameQualifier" Lude..=)
              Lude.<$> distinguishedNameQualifier
          ]
      )
