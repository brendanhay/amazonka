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
-- Module      : Amazonka.CertificateManagerPCA.Types.PolicyQualifierInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.PolicyQualifierInfo where

import Amazonka.CertificateManagerPCA.Types.PolicyQualifierId
import Amazonka.CertificateManagerPCA.Types.Qualifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Modifies the @CertPolicyId@ of a @PolicyInformation@ object with a
-- qualifier. Amazon Web Services Private CA supports the certification
-- practice statement (CPS) qualifier.
--
-- /See:/ 'newPolicyQualifierInfo' smart constructor.
data PolicyQualifierInfo = PolicyQualifierInfo'
  { -- | Identifies the qualifier modifying a @CertPolicyId@.
    policyQualifierId :: PolicyQualifierId,
    -- | Defines the qualifier type. Amazon Web Services Private CA supports the
    -- use of a URI for a CPS qualifier in this field.
    qualifier :: Qualifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyQualifierInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyQualifierId', 'policyQualifierInfo_policyQualifierId' - Identifies the qualifier modifying a @CertPolicyId@.
--
-- 'qualifier', 'policyQualifierInfo_qualifier' - Defines the qualifier type. Amazon Web Services Private CA supports the
-- use of a URI for a CPS qualifier in this field.
newPolicyQualifierInfo ::
  -- | 'policyQualifierId'
  PolicyQualifierId ->
  -- | 'qualifier'
  Qualifier ->
  PolicyQualifierInfo
newPolicyQualifierInfo
  pPolicyQualifierId_
  pQualifier_ =
    PolicyQualifierInfo'
      { policyQualifierId =
          pPolicyQualifierId_,
        qualifier = pQualifier_
      }

-- | Identifies the qualifier modifying a @CertPolicyId@.
policyQualifierInfo_policyQualifierId :: Lens.Lens' PolicyQualifierInfo PolicyQualifierId
policyQualifierInfo_policyQualifierId = Lens.lens (\PolicyQualifierInfo' {policyQualifierId} -> policyQualifierId) (\s@PolicyQualifierInfo' {} a -> s {policyQualifierId = a} :: PolicyQualifierInfo)

-- | Defines the qualifier type. Amazon Web Services Private CA supports the
-- use of a URI for a CPS qualifier in this field.
policyQualifierInfo_qualifier :: Lens.Lens' PolicyQualifierInfo Qualifier
policyQualifierInfo_qualifier = Lens.lens (\PolicyQualifierInfo' {qualifier} -> qualifier) (\s@PolicyQualifierInfo' {} a -> s {qualifier = a} :: PolicyQualifierInfo)

instance Prelude.Hashable PolicyQualifierInfo where
  hashWithSalt _salt PolicyQualifierInfo' {..} =
    _salt
      `Prelude.hashWithSalt` policyQualifierId
      `Prelude.hashWithSalt` qualifier

instance Prelude.NFData PolicyQualifierInfo where
  rnf PolicyQualifierInfo' {..} =
    Prelude.rnf policyQualifierId
      `Prelude.seq` Prelude.rnf qualifier

instance Data.ToJSON PolicyQualifierInfo where
  toJSON PolicyQualifierInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PolicyQualifierId" Data..= policyQualifierId),
            Prelude.Just ("Qualifier" Data..= qualifier)
          ]
      )
