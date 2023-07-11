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
-- Module      : Amazonka.CertificateManagerPCA.Types.PolicyInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.PolicyInformation where

import Amazonka.CertificateManagerPCA.Types.PolicyQualifierInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the X.509 @CertificatePolicies@ extension.
--
-- /See:/ 'newPolicyInformation' smart constructor.
data PolicyInformation = PolicyInformation'
  { -- | Modifies the given @CertPolicyId@ with a qualifier. Amazon Web Services
    -- Private CA supports the certification practice statement (CPS)
    -- qualifier.
    policyQualifiers :: Prelude.Maybe (Prelude.NonEmpty PolicyQualifierInfo),
    -- | Specifies the object identifier (OID) of the certificate policy under
    -- which the certificate was issued. For more information, see NIST\'s
    -- definition of
    -- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
    certPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyQualifiers', 'policyInformation_policyQualifiers' - Modifies the given @CertPolicyId@ with a qualifier. Amazon Web Services
-- Private CA supports the certification practice statement (CPS)
-- qualifier.
--
-- 'certPolicyId', 'policyInformation_certPolicyId' - Specifies the object identifier (OID) of the certificate policy under
-- which the certificate was issued. For more information, see NIST\'s
-- definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
newPolicyInformation ::
  -- | 'certPolicyId'
  Prelude.Text ->
  PolicyInformation
newPolicyInformation pCertPolicyId_ =
  PolicyInformation'
    { policyQualifiers =
        Prelude.Nothing,
      certPolicyId = pCertPolicyId_
    }

-- | Modifies the given @CertPolicyId@ with a qualifier. Amazon Web Services
-- Private CA supports the certification practice statement (CPS)
-- qualifier.
policyInformation_policyQualifiers :: Lens.Lens' PolicyInformation (Prelude.Maybe (Prelude.NonEmpty PolicyQualifierInfo))
policyInformation_policyQualifiers = Lens.lens (\PolicyInformation' {policyQualifiers} -> policyQualifiers) (\s@PolicyInformation' {} a -> s {policyQualifiers = a} :: PolicyInformation) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the object identifier (OID) of the certificate policy under
-- which the certificate was issued. For more information, see NIST\'s
-- definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
policyInformation_certPolicyId :: Lens.Lens' PolicyInformation Prelude.Text
policyInformation_certPolicyId = Lens.lens (\PolicyInformation' {certPolicyId} -> certPolicyId) (\s@PolicyInformation' {} a -> s {certPolicyId = a} :: PolicyInformation)

instance Prelude.Hashable PolicyInformation where
  hashWithSalt _salt PolicyInformation' {..} =
    _salt
      `Prelude.hashWithSalt` policyQualifiers
      `Prelude.hashWithSalt` certPolicyId

instance Prelude.NFData PolicyInformation where
  rnf PolicyInformation' {..} =
    Prelude.rnf policyQualifiers
      `Prelude.seq` Prelude.rnf certPolicyId

instance Data.ToJSON PolicyInformation where
  toJSON PolicyInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyQualifiers" Data..=)
              Prelude.<$> policyQualifiers,
            Prelude.Just ("CertPolicyId" Data..= certPolicyId)
          ]
      )
