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
-- Module      : Amazonka.WAFV2.Types.AWSManagedRulesBotControlRuleSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AWSManagedRulesBotControlRuleSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.InspectionLevel

-- | Details for your use of the Bot Control managed rule group, used in
-- @ManagedRuleGroupConfig@.
--
-- /See:/ 'newAWSManagedRulesBotControlRuleSet' smart constructor.
data AWSManagedRulesBotControlRuleSet = AWSManagedRulesBotControlRuleSet'
  { -- | The inspection level to use for the Bot Control rule group. The common
    -- level is the least expensive. The targeted level includes all common
    -- level rules and adds rules with more advanced inspection criteria. For
    -- details, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>.
    inspectionLevel :: InspectionLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSManagedRulesBotControlRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inspectionLevel', 'aWSManagedRulesBotControlRuleSet_inspectionLevel' - The inspection level to use for the Bot Control rule group. The common
-- level is the least expensive. The targeted level includes all common
-- level rules and adds rules with more advanced inspection criteria. For
-- details, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>.
newAWSManagedRulesBotControlRuleSet ::
  -- | 'inspectionLevel'
  InspectionLevel ->
  AWSManagedRulesBotControlRuleSet
newAWSManagedRulesBotControlRuleSet pInspectionLevel_ =
  AWSManagedRulesBotControlRuleSet'
    { inspectionLevel =
        pInspectionLevel_
    }

-- | The inspection level to use for the Bot Control rule group. The common
-- level is the least expensive. The targeted level includes all common
-- level rules and adds rules with more advanced inspection criteria. For
-- details, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/aws-managed-rule-groups-bot.html WAF Bot Control rule group>.
aWSManagedRulesBotControlRuleSet_inspectionLevel :: Lens.Lens' AWSManagedRulesBotControlRuleSet InspectionLevel
aWSManagedRulesBotControlRuleSet_inspectionLevel = Lens.lens (\AWSManagedRulesBotControlRuleSet' {inspectionLevel} -> inspectionLevel) (\s@AWSManagedRulesBotControlRuleSet' {} a -> s {inspectionLevel = a} :: AWSManagedRulesBotControlRuleSet)

instance
  Core.FromJSON
    AWSManagedRulesBotControlRuleSet
  where
  parseJSON =
    Core.withObject
      "AWSManagedRulesBotControlRuleSet"
      ( \x ->
          AWSManagedRulesBotControlRuleSet'
            Prelude.<$> (x Core..: "InspectionLevel")
      )

instance
  Prelude.Hashable
    AWSManagedRulesBotControlRuleSet
  where
  hashWithSalt
    _salt
    AWSManagedRulesBotControlRuleSet' {..} =
      _salt `Prelude.hashWithSalt` inspectionLevel

instance
  Prelude.NFData
    AWSManagedRulesBotControlRuleSet
  where
  rnf AWSManagedRulesBotControlRuleSet' {..} =
    Prelude.rnf inspectionLevel

instance Core.ToJSON AWSManagedRulesBotControlRuleSet where
  toJSON AWSManagedRulesBotControlRuleSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InspectionLevel" Core..= inspectionLevel)
          ]
      )
