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
-- Module      : Network.AWS.CertificateManagerPCA.Types.Extensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Extensions where

import Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsage
import Network.AWS.CertificateManagerPCA.Types.GeneralName
import Network.AWS.CertificateManagerPCA.Types.KeyUsage
import Network.AWS.CertificateManagerPCA.Types.PolicyInformation
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains X.509 extension information for a certificate.
--
-- /See:/ 'newExtensions' smart constructor.
data Extensions = Extensions'
  { -- | Contains a sequence of one or more policy information terms, each of
    -- which consists of an object identifier (OID) and optional qualifiers.
    -- For more information, see NIST\'s definition of
    -- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
    --
    -- In an end-entity certificate, these terms indicate the policy under
    -- which the certificate was issued and the purposes for which it may be
    -- used. In a CA certificate, these terms limit the set of policies for
    -- certification paths that include this certificate.
    certificatePolicies :: Prelude.Maybe (Prelude.NonEmpty PolicyInformation),
    -- | Specifies additional purposes for which the certified public key may be
    -- used other than basic purposes indicated in the @KeyUsage@ extension.
    extendedKeyUsage :: Prelude.Maybe (Prelude.NonEmpty ExtendedKeyUsage),
    -- | The subject alternative name extension allows identities to be bound to
    -- the subject of the certificate. These identities may be included in
    -- addition to or in place of the identity in the subject field of the
    -- certificate.
    subjectAlternativeNames :: Prelude.Maybe (Prelude.NonEmpty GeneralName),
    keyUsage :: Prelude.Maybe KeyUsage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Extensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificatePolicies', 'extensions_certificatePolicies' - Contains a sequence of one or more policy information terms, each of
-- which consists of an object identifier (OID) and optional qualifiers.
-- For more information, see NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- In an end-entity certificate, these terms indicate the policy under
-- which the certificate was issued and the purposes for which it may be
-- used. In a CA certificate, these terms limit the set of policies for
-- certification paths that include this certificate.
--
-- 'extendedKeyUsage', 'extensions_extendedKeyUsage' - Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
--
-- 'subjectAlternativeNames', 'extensions_subjectAlternativeNames' - The subject alternative name extension allows identities to be bound to
-- the subject of the certificate. These identities may be included in
-- addition to or in place of the identity in the subject field of the
-- certificate.
--
-- 'keyUsage', 'extensions_keyUsage' - Undocumented member.
newExtensions ::
  Extensions
newExtensions =
  Extensions'
    { certificatePolicies = Prelude.Nothing,
      extendedKeyUsage = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      keyUsage = Prelude.Nothing
    }

-- | Contains a sequence of one or more policy information terms, each of
-- which consists of an object identifier (OID) and optional qualifiers.
-- For more information, see NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- In an end-entity certificate, these terms indicate the policy under
-- which the certificate was issued and the purposes for which it may be
-- used. In a CA certificate, these terms limit the set of policies for
-- certification paths that include this certificate.
extensions_certificatePolicies :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty PolicyInformation))
extensions_certificatePolicies = Lens.lens (\Extensions' {certificatePolicies} -> certificatePolicies) (\s@Extensions' {} a -> s {certificatePolicies = a} :: Extensions) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
extensions_extendedKeyUsage :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty ExtendedKeyUsage))
extensions_extendedKeyUsage = Lens.lens (\Extensions' {extendedKeyUsage} -> extendedKeyUsage) (\s@Extensions' {} a -> s {extendedKeyUsage = a} :: Extensions) Prelude.. Lens.mapping Lens._Coerce

-- | The subject alternative name extension allows identities to be bound to
-- the subject of the certificate. These identities may be included in
-- addition to or in place of the identity in the subject field of the
-- certificate.
extensions_subjectAlternativeNames :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty GeneralName))
extensions_subjectAlternativeNames = Lens.lens (\Extensions' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@Extensions' {} a -> s {subjectAlternativeNames = a} :: Extensions) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
extensions_keyUsage :: Lens.Lens' Extensions (Prelude.Maybe KeyUsage)
extensions_keyUsage = Lens.lens (\Extensions' {keyUsage} -> keyUsage) (\s@Extensions' {} a -> s {keyUsage = a} :: Extensions)

instance Prelude.Hashable Extensions

instance Prelude.NFData Extensions

instance Core.ToJSON Extensions where
  toJSON Extensions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CertificatePolicies" Core..=)
              Prelude.<$> certificatePolicies,
            ("ExtendedKeyUsage" Core..=)
              Prelude.<$> extendedKeyUsage,
            ("SubjectAlternativeNames" Core..=)
              Prelude.<$> subjectAlternativeNames,
            ("KeyUsage" Core..=) Prelude.<$> keyUsage
          ]
      )
