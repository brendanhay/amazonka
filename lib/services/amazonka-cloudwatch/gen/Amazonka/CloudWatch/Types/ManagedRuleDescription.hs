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
-- Module      : Amazonka.CloudWatch.Types.ManagedRuleDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.ManagedRuleDescription where

import Amazonka.CloudWatch.Types.ManagedRuleState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about managed Contributor Insights rules, as
-- returned by @ListManagedInsightRules@.
--
-- /See:/ 'newManagedRuleDescription' smart constructor.
data ManagedRuleDescription = ManagedRuleDescription'
  { -- | If a managed rule is enabled, this is the ARN for the related Amazon Web
    -- Services resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | Describes the state of a managed rule. If present, it contains
    -- information about the Contributor Insights rule that contains
    -- information about the related Amazon Web Services resource.
    ruleState :: Prelude.Maybe ManagedRuleState,
    -- | The template name for the managed rule. Used to enable managed rules
    -- using @PutManagedInsightRules@.
    templateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'managedRuleDescription_resourceARN' - If a managed rule is enabled, this is the ARN for the related Amazon Web
-- Services resource.
--
-- 'ruleState', 'managedRuleDescription_ruleState' - Describes the state of a managed rule. If present, it contains
-- information about the Contributor Insights rule that contains
-- information about the related Amazon Web Services resource.
--
-- 'templateName', 'managedRuleDescription_templateName' - The template name for the managed rule. Used to enable managed rules
-- using @PutManagedInsightRules@.
newManagedRuleDescription ::
  ManagedRuleDescription
newManagedRuleDescription =
  ManagedRuleDescription'
    { resourceARN =
        Prelude.Nothing,
      ruleState = Prelude.Nothing,
      templateName = Prelude.Nothing
    }

-- | If a managed rule is enabled, this is the ARN for the related Amazon Web
-- Services resource.
managedRuleDescription_resourceARN :: Lens.Lens' ManagedRuleDescription (Prelude.Maybe Prelude.Text)
managedRuleDescription_resourceARN = Lens.lens (\ManagedRuleDescription' {resourceARN} -> resourceARN) (\s@ManagedRuleDescription' {} a -> s {resourceARN = a} :: ManagedRuleDescription)

-- | Describes the state of a managed rule. If present, it contains
-- information about the Contributor Insights rule that contains
-- information about the related Amazon Web Services resource.
managedRuleDescription_ruleState :: Lens.Lens' ManagedRuleDescription (Prelude.Maybe ManagedRuleState)
managedRuleDescription_ruleState = Lens.lens (\ManagedRuleDescription' {ruleState} -> ruleState) (\s@ManagedRuleDescription' {} a -> s {ruleState = a} :: ManagedRuleDescription)

-- | The template name for the managed rule. Used to enable managed rules
-- using @PutManagedInsightRules@.
managedRuleDescription_templateName :: Lens.Lens' ManagedRuleDescription (Prelude.Maybe Prelude.Text)
managedRuleDescription_templateName = Lens.lens (\ManagedRuleDescription' {templateName} -> templateName) (\s@ManagedRuleDescription' {} a -> s {templateName = a} :: ManagedRuleDescription)

instance Data.FromXML ManagedRuleDescription where
  parseXML x =
    ManagedRuleDescription'
      Prelude.<$> (x Data..@? "ResourceARN")
      Prelude.<*> (x Data..@? "RuleState")
      Prelude.<*> (x Data..@? "TemplateName")

instance Prelude.Hashable ManagedRuleDescription where
  hashWithSalt _salt ManagedRuleDescription' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` ruleState
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ManagedRuleDescription where
  rnf ManagedRuleDescription' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf ruleState
      `Prelude.seq` Prelude.rnf templateName
