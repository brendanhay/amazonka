{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.AggregateComplianceByConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceByConfigRule where

import Network.AWS.Config.Types.Compliance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether an AWS Config rule is compliant based on account ID,
-- region, compliance, and rule name.
--
-- A rule is compliant if all of the resources that the rule evaluated
-- comply with it. It is noncompliant if any of these resources do not
-- comply.
--
-- /See:/ 'newAggregateComplianceByConfigRule' smart constructor.
data AggregateComplianceByConfigRule = AggregateComplianceByConfigRule'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an AWS resource or AWS Config rule is compliant and
    -- provides the number of contributors that affect the compliance.
    compliance :: Prelude.Maybe Compliance,
    -- | The source region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AggregateComplianceByConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'aggregateComplianceByConfigRule_accountId' - The 12-digit account ID of the source account.
--
-- 'configRuleName', 'aggregateComplianceByConfigRule_configRuleName' - The name of the AWS Config rule.
--
-- 'compliance', 'aggregateComplianceByConfigRule_compliance' - Indicates whether an AWS resource or AWS Config rule is compliant and
-- provides the number of contributors that affect the compliance.
--
-- 'awsRegion', 'aggregateComplianceByConfigRule_awsRegion' - The source region from where the data is aggregated.
newAggregateComplianceByConfigRule ::
  AggregateComplianceByConfigRule
newAggregateComplianceByConfigRule =
  AggregateComplianceByConfigRule'
    { accountId =
        Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      compliance = Prelude.Nothing,
      awsRegion = Prelude.Nothing
    }

-- | The 12-digit account ID of the source account.
aggregateComplianceByConfigRule_accountId :: Lens.Lens' AggregateComplianceByConfigRule (Prelude.Maybe Prelude.Text)
aggregateComplianceByConfigRule_accountId = Lens.lens (\AggregateComplianceByConfigRule' {accountId} -> accountId) (\s@AggregateComplianceByConfigRule' {} a -> s {accountId = a} :: AggregateComplianceByConfigRule)

-- | The name of the AWS Config rule.
aggregateComplianceByConfigRule_configRuleName :: Lens.Lens' AggregateComplianceByConfigRule (Prelude.Maybe Prelude.Text)
aggregateComplianceByConfigRule_configRuleName = Lens.lens (\AggregateComplianceByConfigRule' {configRuleName} -> configRuleName) (\s@AggregateComplianceByConfigRule' {} a -> s {configRuleName = a} :: AggregateComplianceByConfigRule)

-- | Indicates whether an AWS resource or AWS Config rule is compliant and
-- provides the number of contributors that affect the compliance.
aggregateComplianceByConfigRule_compliance :: Lens.Lens' AggregateComplianceByConfigRule (Prelude.Maybe Compliance)
aggregateComplianceByConfigRule_compliance = Lens.lens (\AggregateComplianceByConfigRule' {compliance} -> compliance) (\s@AggregateComplianceByConfigRule' {} a -> s {compliance = a} :: AggregateComplianceByConfigRule)

-- | The source region from where the data is aggregated.
aggregateComplianceByConfigRule_awsRegion :: Lens.Lens' AggregateComplianceByConfigRule (Prelude.Maybe Prelude.Text)
aggregateComplianceByConfigRule_awsRegion = Lens.lens (\AggregateComplianceByConfigRule' {awsRegion} -> awsRegion) (\s@AggregateComplianceByConfigRule' {} a -> s {awsRegion = a} :: AggregateComplianceByConfigRule)

instance
  Prelude.FromJSON
    AggregateComplianceByConfigRule
  where
  parseJSON =
    Prelude.withObject
      "AggregateComplianceByConfigRule"
      ( \x ->
          AggregateComplianceByConfigRule'
            Prelude.<$> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "ConfigRuleName")
            Prelude.<*> (x Prelude..:? "Compliance")
            Prelude.<*> (x Prelude..:? "AwsRegion")
      )

instance
  Prelude.Hashable
    AggregateComplianceByConfigRule

instance
  Prelude.NFData
    AggregateComplianceByConfigRule
