{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Types.ASN1Subject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.ASN1Subject where

import Amazonka.CertificateManagerPCA.Types.CustomAttribute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the certificate subject. The @Subject@ field
-- in the certificate identifies the entity that owns or controls the
-- public key in the certificate. The entity can be a user, computer,
-- device, or service. The @Subject @must contain an X.500 distinguished
-- name (DN). A DN is a sequence of relative distinguished names (RDNs).
-- The RDNs are separated by commas in the certificate.
--
-- /See:/ 'newASN1Subject' smart constructor.
data ASN1Subject = ASN1Subject'
  { -- | Two-digit code that specifies the country in which the certificate
    -- subject located.
    country :: Prelude.Maybe Prelude.Text,
    -- | First name.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | State in which the subject of the certificate is located.
    state :: Prelude.Maybe Prelude.Text,
    -- | A subdivision or unit of the organization (such as sales or finance)
    -- with which the certificate subject is affiliated.
    organizationalUnit :: Prelude.Maybe Prelude.Text,
    -- | Typically a qualifier appended to the name of an individual. Examples
    -- include Jr. for junior, Sr. for senior, and III for third.
    generationQualifier :: Prelude.Maybe Prelude.Text,
    -- | Typically a shortened version of a longer __GivenName__. For example,
    -- Jonathan is often shortened to John. Elizabeth is often shortened to
    -- Beth, Liz, or Eliza.
    pseudonym :: Prelude.Maybe Prelude.Text,
    -- | Family name. In the US and the UK, for example, the surname of an
    -- individual is ordered last. In Asian cultures the surname is typically
    -- ordered first.
    surname :: Prelude.Maybe Prelude.Text,
    -- | A title such as Mr. or Ms., which is pre-pended to the name to refer
    -- formally to the certificate subject.
    title :: Prelude.Maybe Prelude.Text,
    -- | Contains a sequence of one or more X.500 relative distinguished names
    -- (RDNs), each of which consists of an object identifier (OID) and a
    -- value. For more information, see NIST’s definition of
    -- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
    --
    -- Custom attributes cannot be used in combination with standard
    -- attributes.
    customAttributes :: Prelude.Maybe (Prelude.NonEmpty CustomAttribute),
    -- | The locality (such as a city or town) in which the certificate subject
    -- is located.
    locality :: Prelude.Maybe Prelude.Text,
    -- | Legal name of the organization with which the certificate subject is
    -- affiliated.
    organization :: Prelude.Maybe Prelude.Text,
    -- | The certificate serial number.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | For CA and end-entity certificates in a private PKI, the common name
    -- (CN) can be any string within the length limit.
    --
    -- Note: In publicly trusted certificates, the common name must be a fully
    -- qualified domain name (FQDN) associated with the certificate subject.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | Concatenation that typically contains the first letter of the
    -- __GivenName__, the first letter of the middle name if one exists, and
    -- the first letter of the __Surname__.
    initials :: Prelude.Maybe Prelude.Text,
    -- | Disambiguating information for the certificate subject.
    distinguishedNameQualifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ASN1Subject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'country', 'aSN1Subject_country' - Two-digit code that specifies the country in which the certificate
-- subject located.
--
-- 'givenName', 'aSN1Subject_givenName' - First name.
--
-- 'state', 'aSN1Subject_state' - State in which the subject of the certificate is located.
--
-- 'organizationalUnit', 'aSN1Subject_organizationalUnit' - A subdivision or unit of the organization (such as sales or finance)
-- with which the certificate subject is affiliated.
--
-- 'generationQualifier', 'aSN1Subject_generationQualifier' - Typically a qualifier appended to the name of an individual. Examples
-- include Jr. for junior, Sr. for senior, and III for third.
--
-- 'pseudonym', 'aSN1Subject_pseudonym' - Typically a shortened version of a longer __GivenName__. For example,
-- Jonathan is often shortened to John. Elizabeth is often shortened to
-- Beth, Liz, or Eliza.
--
-- 'surname', 'aSN1Subject_surname' - Family name. In the US and the UK, for example, the surname of an
-- individual is ordered last. In Asian cultures the surname is typically
-- ordered first.
--
-- 'title', 'aSN1Subject_title' - A title such as Mr. or Ms., which is pre-pended to the name to refer
-- formally to the certificate subject.
--
-- 'customAttributes', 'aSN1Subject_customAttributes' - Contains a sequence of one or more X.500 relative distinguished names
-- (RDNs), each of which consists of an object identifier (OID) and a
-- value. For more information, see NIST’s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- Custom attributes cannot be used in combination with standard
-- attributes.
--
-- 'locality', 'aSN1Subject_locality' - The locality (such as a city or town) in which the certificate subject
-- is located.
--
-- 'organization', 'aSN1Subject_organization' - Legal name of the organization with which the certificate subject is
-- affiliated.
--
-- 'serialNumber', 'aSN1Subject_serialNumber' - The certificate serial number.
--
-- 'commonName', 'aSN1Subject_commonName' - For CA and end-entity certificates in a private PKI, the common name
-- (CN) can be any string within the length limit.
--
-- Note: In publicly trusted certificates, the common name must be a fully
-- qualified domain name (FQDN) associated with the certificate subject.
--
-- 'initials', 'aSN1Subject_initials' - Concatenation that typically contains the first letter of the
-- __GivenName__, the first letter of the middle name if one exists, and
-- the first letter of the __Surname__.
--
-- 'distinguishedNameQualifier', 'aSN1Subject_distinguishedNameQualifier' - Disambiguating information for the certificate subject.
newASN1Subject ::
  ASN1Subject
newASN1Subject =
  ASN1Subject'
    { country = Prelude.Nothing,
      givenName = Prelude.Nothing,
      state = Prelude.Nothing,
      organizationalUnit = Prelude.Nothing,
      generationQualifier = Prelude.Nothing,
      pseudonym = Prelude.Nothing,
      surname = Prelude.Nothing,
      title = Prelude.Nothing,
      customAttributes = Prelude.Nothing,
      locality = Prelude.Nothing,
      organization = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      commonName = Prelude.Nothing,
      initials = Prelude.Nothing,
      distinguishedNameQualifier = Prelude.Nothing
    }

-- | Two-digit code that specifies the country in which the certificate
-- subject located.
aSN1Subject_country :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_country = Lens.lens (\ASN1Subject' {country} -> country) (\s@ASN1Subject' {} a -> s {country = a} :: ASN1Subject)

-- | First name.
aSN1Subject_givenName :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_givenName = Lens.lens (\ASN1Subject' {givenName} -> givenName) (\s@ASN1Subject' {} a -> s {givenName = a} :: ASN1Subject)

-- | State in which the subject of the certificate is located.
aSN1Subject_state :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_state = Lens.lens (\ASN1Subject' {state} -> state) (\s@ASN1Subject' {} a -> s {state = a} :: ASN1Subject)

-- | A subdivision or unit of the organization (such as sales or finance)
-- with which the certificate subject is affiliated.
aSN1Subject_organizationalUnit :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_organizationalUnit = Lens.lens (\ASN1Subject' {organizationalUnit} -> organizationalUnit) (\s@ASN1Subject' {} a -> s {organizationalUnit = a} :: ASN1Subject)

-- | Typically a qualifier appended to the name of an individual. Examples
-- include Jr. for junior, Sr. for senior, and III for third.
aSN1Subject_generationQualifier :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_generationQualifier = Lens.lens (\ASN1Subject' {generationQualifier} -> generationQualifier) (\s@ASN1Subject' {} a -> s {generationQualifier = a} :: ASN1Subject)

-- | Typically a shortened version of a longer __GivenName__. For example,
-- Jonathan is often shortened to John. Elizabeth is often shortened to
-- Beth, Liz, or Eliza.
aSN1Subject_pseudonym :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_pseudonym = Lens.lens (\ASN1Subject' {pseudonym} -> pseudonym) (\s@ASN1Subject' {} a -> s {pseudonym = a} :: ASN1Subject)

-- | Family name. In the US and the UK, for example, the surname of an
-- individual is ordered last. In Asian cultures the surname is typically
-- ordered first.
aSN1Subject_surname :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_surname = Lens.lens (\ASN1Subject' {surname} -> surname) (\s@ASN1Subject' {} a -> s {surname = a} :: ASN1Subject)

-- | A title such as Mr. or Ms., which is pre-pended to the name to refer
-- formally to the certificate subject.
aSN1Subject_title :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_title = Lens.lens (\ASN1Subject' {title} -> title) (\s@ASN1Subject' {} a -> s {title = a} :: ASN1Subject)

-- | Contains a sequence of one or more X.500 relative distinguished names
-- (RDNs), each of which consists of an object identifier (OID) and a
-- value. For more information, see NIST’s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- Custom attributes cannot be used in combination with standard
-- attributes.
aSN1Subject_customAttributes :: Lens.Lens' ASN1Subject (Prelude.Maybe (Prelude.NonEmpty CustomAttribute))
aSN1Subject_customAttributes = Lens.lens (\ASN1Subject' {customAttributes} -> customAttributes) (\s@ASN1Subject' {} a -> s {customAttributes = a} :: ASN1Subject) Prelude.. Lens.mapping Lens.coerced

-- | The locality (such as a city or town) in which the certificate subject
-- is located.
aSN1Subject_locality :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_locality = Lens.lens (\ASN1Subject' {locality} -> locality) (\s@ASN1Subject' {} a -> s {locality = a} :: ASN1Subject)

-- | Legal name of the organization with which the certificate subject is
-- affiliated.
aSN1Subject_organization :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_organization = Lens.lens (\ASN1Subject' {organization} -> organization) (\s@ASN1Subject' {} a -> s {organization = a} :: ASN1Subject)

-- | The certificate serial number.
aSN1Subject_serialNumber :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_serialNumber = Lens.lens (\ASN1Subject' {serialNumber} -> serialNumber) (\s@ASN1Subject' {} a -> s {serialNumber = a} :: ASN1Subject)

-- | For CA and end-entity certificates in a private PKI, the common name
-- (CN) can be any string within the length limit.
--
-- Note: In publicly trusted certificates, the common name must be a fully
-- qualified domain name (FQDN) associated with the certificate subject.
aSN1Subject_commonName :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_commonName = Lens.lens (\ASN1Subject' {commonName} -> commonName) (\s@ASN1Subject' {} a -> s {commonName = a} :: ASN1Subject)

-- | Concatenation that typically contains the first letter of the
-- __GivenName__, the first letter of the middle name if one exists, and
-- the first letter of the __Surname__.
aSN1Subject_initials :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_initials = Lens.lens (\ASN1Subject' {initials} -> initials) (\s@ASN1Subject' {} a -> s {initials = a} :: ASN1Subject)

-- | Disambiguating information for the certificate subject.
aSN1Subject_distinguishedNameQualifier :: Lens.Lens' ASN1Subject (Prelude.Maybe Prelude.Text)
aSN1Subject_distinguishedNameQualifier = Lens.lens (\ASN1Subject' {distinguishedNameQualifier} -> distinguishedNameQualifier) (\s@ASN1Subject' {} a -> s {distinguishedNameQualifier = a} :: ASN1Subject)

instance Core.FromJSON ASN1Subject where
  parseJSON =
    Core.withObject
      "ASN1Subject"
      ( \x ->
          ASN1Subject'
            Prelude.<$> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "GivenName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "OrganizationalUnit")
            Prelude.<*> (x Core..:? "GenerationQualifier")
            Prelude.<*> (x Core..:? "Pseudonym")
            Prelude.<*> (x Core..:? "Surname")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "CustomAttributes")
            Prelude.<*> (x Core..:? "Locality")
            Prelude.<*> (x Core..:? "Organization")
            Prelude.<*> (x Core..:? "SerialNumber")
            Prelude.<*> (x Core..:? "CommonName")
            Prelude.<*> (x Core..:? "Initials")
            Prelude.<*> (x Core..:? "DistinguishedNameQualifier")
      )

instance Prelude.Hashable ASN1Subject where
  hashWithSalt _salt ASN1Subject' {..} =
    _salt `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` organizationalUnit
      `Prelude.hashWithSalt` generationQualifier
      `Prelude.hashWithSalt` pseudonym
      `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` customAttributes
      `Prelude.hashWithSalt` locality
      `Prelude.hashWithSalt` organization
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` commonName
      `Prelude.hashWithSalt` initials
      `Prelude.hashWithSalt` distinguishedNameQualifier

instance Prelude.NFData ASN1Subject where
  rnf ASN1Subject' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf organizationalUnit
      `Prelude.seq` Prelude.rnf generationQualifier
      `Prelude.seq` Prelude.rnf pseudonym
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf customAttributes
      `Prelude.seq` Prelude.rnf locality
      `Prelude.seq` Prelude.rnf organization
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf commonName
      `Prelude.seq` Prelude.rnf initials
      `Prelude.seq` Prelude.rnf distinguishedNameQualifier

instance Core.ToJSON ASN1Subject where
  toJSON ASN1Subject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Country" Core..=) Prelude.<$> country,
            ("GivenName" Core..=) Prelude.<$> givenName,
            ("State" Core..=) Prelude.<$> state,
            ("OrganizationalUnit" Core..=)
              Prelude.<$> organizationalUnit,
            ("GenerationQualifier" Core..=)
              Prelude.<$> generationQualifier,
            ("Pseudonym" Core..=) Prelude.<$> pseudonym,
            ("Surname" Core..=) Prelude.<$> surname,
            ("Title" Core..=) Prelude.<$> title,
            ("CustomAttributes" Core..=)
              Prelude.<$> customAttributes,
            ("Locality" Core..=) Prelude.<$> locality,
            ("Organization" Core..=) Prelude.<$> organization,
            ("SerialNumber" Core..=) Prelude.<$> serialNumber,
            ("CommonName" Core..=) Prelude.<$> commonName,
            ("Initials" Core..=) Prelude.<$> initials,
            ("DistinguishedNameQualifier" Core..=)
              Prelude.<$> distinguishedNameQualifier
          ]
      )
