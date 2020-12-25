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
    asnsCommonName,
    asnsCountry,
    asnsDistinguishedNameQualifier,
    asnsGenerationQualifier,
    asnsGivenName,
    asnsInitials,
    asnsLocality,
    asnsOrganization,
    asnsOrganizationalUnit,
    asnsPseudonym,
    asnsSerialNumber,
    asnsState,
    asnsSurname,
    asnsTitle,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types.CommonName as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Country as Types
import qualified Network.AWS.CertificateManagerPCA.Types.DistinguishedNameQualifier as Types
import qualified Network.AWS.CertificateManagerPCA.Types.GenerationQualifier as Types
import qualified Network.AWS.CertificateManagerPCA.Types.GivenName as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Organization as Types
import qualified Network.AWS.CertificateManagerPCA.Types.OrganizationalUnit as Types
import qualified Network.AWS.CertificateManagerPCA.Types.SerialNumber as Types
import qualified Network.AWS.CertificateManagerPCA.Types.String128 as Types
import qualified Network.AWS.CertificateManagerPCA.Types.String5 as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Surname as Types
import qualified Network.AWS.CertificateManagerPCA.Types.Title as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the certificate subject. The certificate can be one issued by your private certificate authority (CA) or it can be your private CA certificate. The __Subject__ field in the certificate identifies the entity that owns or controls the public key in the certificate. The entity can be a user, computer, device, or service. The __Subject__ must contain an X.500 distinguished name (DN). A DN is a sequence of relative distinguished names (RDNs). The RDNs are separated by commas in the certificate. The DN must be unique for each entity, but your private CA can issue more than one certificate with the same DN to the same entity.
--
-- /See:/ 'mkASN1Subject' smart constructor.
data ASN1Subject = ASN1Subject'
  { -- | Fully qualified domain name (FQDN) associated with the certificate subject.
    commonName :: Core.Maybe Types.CommonName,
    -- | Two-digit code that specifies the country in which the certificate subject located.
    country :: Core.Maybe Types.Country,
    -- | Disambiguating information for the certificate subject.
    distinguishedNameQualifier :: Core.Maybe Types.DistinguishedNameQualifier,
    -- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
    generationQualifier :: Core.Maybe Types.GenerationQualifier,
    -- | First name.
    givenName :: Core.Maybe Types.GivenName,
    -- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
    initials :: Core.Maybe Types.String5,
    -- | The locality (such as a city or town) in which the certificate subject is located.
    locality :: Core.Maybe Types.String128,
    -- | Legal name of the organization with which the certificate subject is affiliated.
    organization :: Core.Maybe Types.Organization,
    -- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
    organizationalUnit :: Core.Maybe Types.OrganizationalUnit,
    -- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
    pseudonym :: Core.Maybe Types.String128,
    -- | The certificate serial number.
    serialNumber :: Core.Maybe Types.SerialNumber,
    -- | State in which the subject of the certificate is located.
    state :: Core.Maybe Types.String128,
    -- | Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
    surname :: Core.Maybe Types.Surname,
    -- | A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
    title :: Core.Maybe Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ASN1Subject' value with any optional fields omitted.
mkASN1Subject ::
  ASN1Subject
mkASN1Subject =
  ASN1Subject'
    { commonName = Core.Nothing,
      country = Core.Nothing,
      distinguishedNameQualifier = Core.Nothing,
      generationQualifier = Core.Nothing,
      givenName = Core.Nothing,
      initials = Core.Nothing,
      locality = Core.Nothing,
      organization = Core.Nothing,
      organizationalUnit = Core.Nothing,
      pseudonym = Core.Nothing,
      serialNumber = Core.Nothing,
      state = Core.Nothing,
      surname = Core.Nothing,
      title = Core.Nothing
    }

-- | Fully qualified domain name (FQDN) associated with the certificate subject.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsCommonName :: Lens.Lens' ASN1Subject (Core.Maybe Types.CommonName)
asnsCommonName = Lens.field @"commonName"
{-# DEPRECATED asnsCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

-- | Two-digit code that specifies the country in which the certificate subject located.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsCountry :: Lens.Lens' ASN1Subject (Core.Maybe Types.Country)
asnsCountry = Lens.field @"country"
{-# DEPRECATED asnsCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | Disambiguating information for the certificate subject.
--
-- /Note:/ Consider using 'distinguishedNameQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsDistinguishedNameQualifier :: Lens.Lens' ASN1Subject (Core.Maybe Types.DistinguishedNameQualifier)
asnsDistinguishedNameQualifier = Lens.field @"distinguishedNameQualifier"
{-# DEPRECATED asnsDistinguishedNameQualifier "Use generic-lens or generic-optics with 'distinguishedNameQualifier' instead." #-}

-- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
--
-- /Note:/ Consider using 'generationQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsGenerationQualifier :: Lens.Lens' ASN1Subject (Core.Maybe Types.GenerationQualifier)
asnsGenerationQualifier = Lens.field @"generationQualifier"
{-# DEPRECATED asnsGenerationQualifier "Use generic-lens or generic-optics with 'generationQualifier' instead." #-}

-- | First name.
--
-- /Note:/ Consider using 'givenName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsGivenName :: Lens.Lens' ASN1Subject (Core.Maybe Types.GivenName)
asnsGivenName = Lens.field @"givenName"
{-# DEPRECATED asnsGivenName "Use generic-lens or generic-optics with 'givenName' instead." #-}

-- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
--
-- /Note:/ Consider using 'initials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsInitials :: Lens.Lens' ASN1Subject (Core.Maybe Types.String5)
asnsInitials = Lens.field @"initials"
{-# DEPRECATED asnsInitials "Use generic-lens or generic-optics with 'initials' instead." #-}

-- | The locality (such as a city or town) in which the certificate subject is located.
--
-- /Note:/ Consider using 'locality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsLocality :: Lens.Lens' ASN1Subject (Core.Maybe Types.String128)
asnsLocality = Lens.field @"locality"
{-# DEPRECATED asnsLocality "Use generic-lens or generic-optics with 'locality' instead." #-}

-- | Legal name of the organization with which the certificate subject is affiliated.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsOrganization :: Lens.Lens' ASN1Subject (Core.Maybe Types.Organization)
asnsOrganization = Lens.field @"organization"
{-# DEPRECATED asnsOrganization "Use generic-lens or generic-optics with 'organization' instead." #-}

-- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsOrganizationalUnit :: Lens.Lens' ASN1Subject (Core.Maybe Types.OrganizationalUnit)
asnsOrganizationalUnit = Lens.field @"organizationalUnit"
{-# DEPRECATED asnsOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
--
-- /Note:/ Consider using 'pseudonym' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsPseudonym :: Lens.Lens' ASN1Subject (Core.Maybe Types.String128)
asnsPseudonym = Lens.field @"pseudonym"
{-# DEPRECATED asnsPseudonym "Use generic-lens or generic-optics with 'pseudonym' instead." #-}

-- | The certificate serial number.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsSerialNumber :: Lens.Lens' ASN1Subject (Core.Maybe Types.SerialNumber)
asnsSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED asnsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | State in which the subject of the certificate is located.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsState :: Lens.Lens' ASN1Subject (Core.Maybe Types.String128)
asnsState = Lens.field @"state"
{-# DEPRECATED asnsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
--
-- /Note:/ Consider using 'surname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsSurname :: Lens.Lens' ASN1Subject (Core.Maybe Types.Surname)
asnsSurname = Lens.field @"surname"
{-# DEPRECATED asnsSurname "Use generic-lens or generic-optics with 'surname' instead." #-}

-- | A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asnsTitle :: Lens.Lens' ASN1Subject (Core.Maybe Types.Title)
asnsTitle = Lens.field @"title"
{-# DEPRECATED asnsTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON ASN1Subject where
  toJSON ASN1Subject {..} =
    Core.object
      ( Core.catMaybes
          [ ("CommonName" Core..=) Core.<$> commonName,
            ("Country" Core..=) Core.<$> country,
            ("DistinguishedNameQualifier" Core..=)
              Core.<$> distinguishedNameQualifier,
            ("GenerationQualifier" Core..=) Core.<$> generationQualifier,
            ("GivenName" Core..=) Core.<$> givenName,
            ("Initials" Core..=) Core.<$> initials,
            ("Locality" Core..=) Core.<$> locality,
            ("Organization" Core..=) Core.<$> organization,
            ("OrganizationalUnit" Core..=) Core.<$> organizationalUnit,
            ("Pseudonym" Core..=) Core.<$> pseudonym,
            ("SerialNumber" Core..=) Core.<$> serialNumber,
            ("State" Core..=) Core.<$> state,
            ("Surname" Core..=) Core.<$> surname,
            ("Title" Core..=) Core.<$> title
          ]
      )

instance Core.FromJSON ASN1Subject where
  parseJSON =
    Core.withObject "ASN1Subject" Core.$
      \x ->
        ASN1Subject'
          Core.<$> (x Core..:? "CommonName")
          Core.<*> (x Core..:? "Country")
          Core.<*> (x Core..:? "DistinguishedNameQualifier")
          Core.<*> (x Core..:? "GenerationQualifier")
          Core.<*> (x Core..:? "GivenName")
          Core.<*> (x Core..:? "Initials")
          Core.<*> (x Core..:? "Locality")
          Core.<*> (x Core..:? "Organization")
          Core.<*> (x Core..:? "OrganizationalUnit")
          Core.<*> (x Core..:? "Pseudonym")
          Core.<*> (x Core..:? "SerialNumber")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "Surname")
          Core.<*> (x Core..:? "Title")
