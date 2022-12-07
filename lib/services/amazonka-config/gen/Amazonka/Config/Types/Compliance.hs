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
-- Module      : Amazonka.Config.Types.Compliance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.Compliance where

import Amazonka.Config.Types.ComplianceContributorCount
import Amazonka.Config.Types.ComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether an Amazon Web Services resource or Config rule is
-- compliant and provides the number of contributors that affect the
-- compliance.
--
-- /See:/ 'newCompliance' smart constructor.
data Compliance = Compliance'
  { -- | The number of Amazon Web Services resources or Config rules that cause a
    -- result of @NON_COMPLIANT@, up to a maximum number.
    complianceContributorCount :: Prelude.Maybe ComplianceContributorCount,
    -- | Indicates whether an Amazon Web Services resource or Config rule is
    -- compliant.
    --
    -- A resource is compliant if it complies with all of the Config rules that
    -- evaluate it. A resource is noncompliant if it does not comply with one
    -- or more of these rules.
    --
    -- A rule is compliant if all of the resources that the rule evaluates
    -- comply with it. A rule is noncompliant if any of these resources do not
    -- comply.
    --
    -- Config returns the @INSUFFICIENT_DATA@ value when no evaluation results
    -- are available for the Amazon Web Services resource or Config rule.
    --
    -- For the @Compliance@ data type, Config supports only @COMPLIANT@,
    -- @NON_COMPLIANT@, and @INSUFFICIENT_DATA@ values. Config does not support
    -- the @NOT_APPLICABLE@ value for the @Compliance@ data type.
    complianceType :: Prelude.Maybe ComplianceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Compliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceContributorCount', 'compliance_complianceContributorCount' - The number of Amazon Web Services resources or Config rules that cause a
-- result of @NON_COMPLIANT@, up to a maximum number.
--
-- 'complianceType', 'compliance_complianceType' - Indicates whether an Amazon Web Services resource or Config rule is
-- compliant.
--
-- A resource is compliant if it complies with all of the Config rules that
-- evaluate it. A resource is noncompliant if it does not comply with one
-- or more of these rules.
--
-- A rule is compliant if all of the resources that the rule evaluates
-- comply with it. A rule is noncompliant if any of these resources do not
-- comply.
--
-- Config returns the @INSUFFICIENT_DATA@ value when no evaluation results
-- are available for the Amazon Web Services resource or Config rule.
--
-- For the @Compliance@ data type, Config supports only @COMPLIANT@,
-- @NON_COMPLIANT@, and @INSUFFICIENT_DATA@ values. Config does not support
-- the @NOT_APPLICABLE@ value for the @Compliance@ data type.
newCompliance ::
  Compliance
newCompliance =
  Compliance'
    { complianceContributorCount =
        Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

-- | The number of Amazon Web Services resources or Config rules that cause a
-- result of @NON_COMPLIANT@, up to a maximum number.
compliance_complianceContributorCount :: Lens.Lens' Compliance (Prelude.Maybe ComplianceContributorCount)
compliance_complianceContributorCount = Lens.lens (\Compliance' {complianceContributorCount} -> complianceContributorCount) (\s@Compliance' {} a -> s {complianceContributorCount = a} :: Compliance)

-- | Indicates whether an Amazon Web Services resource or Config rule is
-- compliant.
--
-- A resource is compliant if it complies with all of the Config rules that
-- evaluate it. A resource is noncompliant if it does not comply with one
-- or more of these rules.
--
-- A rule is compliant if all of the resources that the rule evaluates
-- comply with it. A rule is noncompliant if any of these resources do not
-- comply.
--
-- Config returns the @INSUFFICIENT_DATA@ value when no evaluation results
-- are available for the Amazon Web Services resource or Config rule.
--
-- For the @Compliance@ data type, Config supports only @COMPLIANT@,
-- @NON_COMPLIANT@, and @INSUFFICIENT_DATA@ values. Config does not support
-- the @NOT_APPLICABLE@ value for the @Compliance@ data type.
compliance_complianceType :: Lens.Lens' Compliance (Prelude.Maybe ComplianceType)
compliance_complianceType = Lens.lens (\Compliance' {complianceType} -> complianceType) (\s@Compliance' {} a -> s {complianceType = a} :: Compliance)

instance Data.FromJSON Compliance where
  parseJSON =
    Data.withObject
      "Compliance"
      ( \x ->
          Compliance'
            Prelude.<$> (x Data..:? "ComplianceContributorCount")
            Prelude.<*> (x Data..:? "ComplianceType")
      )

instance Prelude.Hashable Compliance where
  hashWithSalt _salt Compliance' {..} =
    _salt
      `Prelude.hashWithSalt` complianceContributorCount
      `Prelude.hashWithSalt` complianceType

instance Prelude.NFData Compliance where
  rnf Compliance' {..} =
    Prelude.rnf complianceContributorCount
      `Prelude.seq` Prelude.rnf complianceType
