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
-- Module      : Amazonka.Config.Types.CustomPolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.CustomPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the runtime system, policy definition, and whether debug
-- logging enabled. You can specify the following CustomPolicyDetails
-- parameter values only for Config Custom Policy rules.
--
-- /See:/ 'newCustomPolicyDetails' smart constructor.
data CustomPolicyDetails = CustomPolicyDetails'
  { -- | The boolean expression for enabling debug logging for your Config Custom
    -- Policy rule. The default value is @false@.
    enableDebugLogDelivery :: Prelude.Maybe Prelude.Bool,
    -- | The runtime system for your Config Custom Policy rule. Guard is a
    -- policy-as-code language that allows you to write policies that are
    -- enforced by Config Custom Policy rules. For more information about
    -- Guard, see the
    -- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
    policyRuntime :: Prelude.Text,
    -- | The policy definition containing the logic for your Config Custom Policy
    -- rule.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableDebugLogDelivery', 'customPolicyDetails_enableDebugLogDelivery' - The boolean expression for enabling debug logging for your Config Custom
-- Policy rule. The default value is @false@.
--
-- 'policyRuntime', 'customPolicyDetails_policyRuntime' - The runtime system for your Config Custom Policy rule. Guard is a
-- policy-as-code language that allows you to write policies that are
-- enforced by Config Custom Policy rules. For more information about
-- Guard, see the
-- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
--
-- 'policyText', 'customPolicyDetails_policyText' - The policy definition containing the logic for your Config Custom Policy
-- rule.
newCustomPolicyDetails ::
  -- | 'policyRuntime'
  Prelude.Text ->
  -- | 'policyText'
  Prelude.Text ->
  CustomPolicyDetails
newCustomPolicyDetails pPolicyRuntime_ pPolicyText_ =
  CustomPolicyDetails'
    { enableDebugLogDelivery =
        Prelude.Nothing,
      policyRuntime = pPolicyRuntime_,
      policyText = pPolicyText_
    }

-- | The boolean expression for enabling debug logging for your Config Custom
-- Policy rule. The default value is @false@.
customPolicyDetails_enableDebugLogDelivery :: Lens.Lens' CustomPolicyDetails (Prelude.Maybe Prelude.Bool)
customPolicyDetails_enableDebugLogDelivery = Lens.lens (\CustomPolicyDetails' {enableDebugLogDelivery} -> enableDebugLogDelivery) (\s@CustomPolicyDetails' {} a -> s {enableDebugLogDelivery = a} :: CustomPolicyDetails)

-- | The runtime system for your Config Custom Policy rule. Guard is a
-- policy-as-code language that allows you to write policies that are
-- enforced by Config Custom Policy rules. For more information about
-- Guard, see the
-- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
customPolicyDetails_policyRuntime :: Lens.Lens' CustomPolicyDetails Prelude.Text
customPolicyDetails_policyRuntime = Lens.lens (\CustomPolicyDetails' {policyRuntime} -> policyRuntime) (\s@CustomPolicyDetails' {} a -> s {policyRuntime = a} :: CustomPolicyDetails)

-- | The policy definition containing the logic for your Config Custom Policy
-- rule.
customPolicyDetails_policyText :: Lens.Lens' CustomPolicyDetails Prelude.Text
customPolicyDetails_policyText = Lens.lens (\CustomPolicyDetails' {policyText} -> policyText) (\s@CustomPolicyDetails' {} a -> s {policyText = a} :: CustomPolicyDetails)

instance Data.FromJSON CustomPolicyDetails where
  parseJSON =
    Data.withObject
      "CustomPolicyDetails"
      ( \x ->
          CustomPolicyDetails'
            Prelude.<$> (x Data..:? "EnableDebugLogDelivery")
            Prelude.<*> (x Data..: "PolicyRuntime")
            Prelude.<*> (x Data..: "PolicyText")
      )

instance Prelude.Hashable CustomPolicyDetails where
  hashWithSalt _salt CustomPolicyDetails' {..} =
    _salt `Prelude.hashWithSalt` enableDebugLogDelivery
      `Prelude.hashWithSalt` policyRuntime
      `Prelude.hashWithSalt` policyText

instance Prelude.NFData CustomPolicyDetails where
  rnf CustomPolicyDetails' {..} =
    Prelude.rnf enableDebugLogDelivery
      `Prelude.seq` Prelude.rnf policyRuntime
      `Prelude.seq` Prelude.rnf policyText

instance Data.ToJSON CustomPolicyDetails where
  toJSON CustomPolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableDebugLogDelivery" Data..=)
              Prelude.<$> enableDebugLogDelivery,
            Prelude.Just ("PolicyRuntime" Data..= policyRuntime),
            Prelude.Just ("PolicyText" Data..= policyText)
          ]
      )
