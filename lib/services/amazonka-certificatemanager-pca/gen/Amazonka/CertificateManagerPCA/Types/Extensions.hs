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
-- Module      : Amazonka.CertificateManagerPCA.Types.Extensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.Extensions where

import Amazonka.CertificateManagerPCA.Types.CustomExtension
import Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsage
import Amazonka.CertificateManagerPCA.Types.GeneralName
import Amazonka.CertificateManagerPCA.Types.KeyUsage
import Amazonka.CertificateManagerPCA.Types.PolicyInformation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    -- | Contains a sequence of one or more X.509 extensions, each of which
    -- consists of an object identifier (OID), a base64-encoded value, and the
    -- critical flag. For more information, see the
    -- <https://oidref.com/2.5.29 Global OID reference database.>
    customExtensions :: Prelude.Maybe (Prelude.NonEmpty CustomExtension),
    -- | Specifies additional purposes for which the certified public key may be
    -- used other than basic purposes indicated in the @KeyUsage@ extension.
    extendedKeyUsage :: Prelude.Maybe (Prelude.NonEmpty ExtendedKeyUsage),
    keyUsage :: Prelude.Maybe KeyUsage,
    -- | The subject alternative name extension allows identities to be bound to
    -- the subject of the certificate. These identities may be included in
    -- addition to or in place of the identity in the subject field of the
    -- certificate.
    subjectAlternativeNames :: Prelude.Maybe (Prelude.NonEmpty GeneralName)
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
-- 'customExtensions', 'extensions_customExtensions' - Contains a sequence of one or more X.509 extensions, each of which
-- consists of an object identifier (OID), a base64-encoded value, and the
-- critical flag. For more information, see the
-- <https://oidref.com/2.5.29 Global OID reference database.>
--
-- 'extendedKeyUsage', 'extensions_extendedKeyUsage' - Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
--
-- 'keyUsage', 'extensions_keyUsage' - Undocumented member.
--
-- 'subjectAlternativeNames', 'extensions_subjectAlternativeNames' - The subject alternative name extension allows identities to be bound to
-- the subject of the certificate. These identities may be included in
-- addition to or in place of the identity in the subject field of the
-- certificate.
newExtensions ::
  Extensions
newExtensions =
  Extensions'
    { certificatePolicies = Prelude.Nothing,
      customExtensions = Prelude.Nothing,
      extendedKeyUsage = Prelude.Nothing,
      keyUsage = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing
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
extensions_certificatePolicies = Lens.lens (\Extensions' {certificatePolicies} -> certificatePolicies) (\s@Extensions' {} a -> s {certificatePolicies = a} :: Extensions) Prelude.. Lens.mapping Lens.coerced

-- | Contains a sequence of one or more X.509 extensions, each of which
-- consists of an object identifier (OID), a base64-encoded value, and the
-- critical flag. For more information, see the
-- <https://oidref.com/2.5.29 Global OID reference database.>
extensions_customExtensions :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty CustomExtension))
extensions_customExtensions = Lens.lens (\Extensions' {customExtensions} -> customExtensions) (\s@Extensions' {} a -> s {customExtensions = a} :: Extensions) Prelude.. Lens.mapping Lens.coerced

-- | Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
extensions_extendedKeyUsage :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty ExtendedKeyUsage))
extensions_extendedKeyUsage = Lens.lens (\Extensions' {extendedKeyUsage} -> extendedKeyUsage) (\s@Extensions' {} a -> s {extendedKeyUsage = a} :: Extensions) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
extensions_keyUsage :: Lens.Lens' Extensions (Prelude.Maybe KeyUsage)
extensions_keyUsage = Lens.lens (\Extensions' {keyUsage} -> keyUsage) (\s@Extensions' {} a -> s {keyUsage = a} :: Extensions)

-- | The subject alternative name extension allows identities to be bound to
-- the subject of the certificate. These identities may be included in
-- addition to or in place of the identity in the subject field of the
-- certificate.
extensions_subjectAlternativeNames :: Lens.Lens' Extensions (Prelude.Maybe (Prelude.NonEmpty GeneralName))
extensions_subjectAlternativeNames = Lens.lens (\Extensions' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@Extensions' {} a -> s {subjectAlternativeNames = a} :: Extensions) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Extensions where
  hashWithSalt _salt Extensions' {..} =
    _salt
      `Prelude.hashWithSalt` certificatePolicies
      `Prelude.hashWithSalt` customExtensions
      `Prelude.hashWithSalt` extendedKeyUsage
      `Prelude.hashWithSalt` keyUsage
      `Prelude.hashWithSalt` subjectAlternativeNames

instance Prelude.NFData Extensions where
  rnf Extensions' {..} =
    Prelude.rnf certificatePolicies
      `Prelude.seq` Prelude.rnf customExtensions
      `Prelude.seq` Prelude.rnf extendedKeyUsage
      `Prelude.seq` Prelude.rnf keyUsage
      `Prelude.seq` Prelude.rnf subjectAlternativeNames

instance Data.ToJSON Extensions where
  toJSON Extensions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificatePolicies" Data..=)
              Prelude.<$> certificatePolicies,
            ("CustomExtensions" Data..=)
              Prelude.<$> customExtensions,
            ("ExtendedKeyUsage" Data..=)
              Prelude.<$> extendedKeyUsage,
            ("KeyUsage" Data..=) Prelude.<$> keyUsage,
            ("SubjectAlternativeNames" Data..=)
              Prelude.<$> subjectAlternativeNames
          ]
      )
