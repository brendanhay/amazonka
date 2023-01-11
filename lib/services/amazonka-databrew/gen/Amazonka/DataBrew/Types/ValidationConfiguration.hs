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
-- Module      : Amazonka.DataBrew.Types.ValidationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ValidationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.ValidationMode
import qualified Amazonka.Prelude as Prelude

-- | Configuration for data quality validation. Used to select the Rulesets
-- and Validation Mode to be used in the profile job. When
-- ValidationConfiguration is null, the profile job will run without data
-- quality validation.
--
-- /See:/ 'newValidationConfiguration' smart constructor.
data ValidationConfiguration = ValidationConfiguration'
  { -- | Mode of data quality validation. Default mode is “CHECK_ALL” which
    -- verifies all rules defined in the selected ruleset.
    validationMode :: Prelude.Maybe ValidationMode,
    -- | The Amazon Resource Name (ARN) for the ruleset to be validated in the
    -- profile job. The TargetArn of the selected ruleset should be the same as
    -- the Amazon Resource Name (ARN) of the dataset that is associated with
    -- the profile job.
    rulesetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationMode', 'validationConfiguration_validationMode' - Mode of data quality validation. Default mode is “CHECK_ALL” which
-- verifies all rules defined in the selected ruleset.
--
-- 'rulesetArn', 'validationConfiguration_rulesetArn' - The Amazon Resource Name (ARN) for the ruleset to be validated in the
-- profile job. The TargetArn of the selected ruleset should be the same as
-- the Amazon Resource Name (ARN) of the dataset that is associated with
-- the profile job.
newValidationConfiguration ::
  -- | 'rulesetArn'
  Prelude.Text ->
  ValidationConfiguration
newValidationConfiguration pRulesetArn_ =
  ValidationConfiguration'
    { validationMode =
        Prelude.Nothing,
      rulesetArn = pRulesetArn_
    }

-- | Mode of data quality validation. Default mode is “CHECK_ALL” which
-- verifies all rules defined in the selected ruleset.
validationConfiguration_validationMode :: Lens.Lens' ValidationConfiguration (Prelude.Maybe ValidationMode)
validationConfiguration_validationMode = Lens.lens (\ValidationConfiguration' {validationMode} -> validationMode) (\s@ValidationConfiguration' {} a -> s {validationMode = a} :: ValidationConfiguration)

-- | The Amazon Resource Name (ARN) for the ruleset to be validated in the
-- profile job. The TargetArn of the selected ruleset should be the same as
-- the Amazon Resource Name (ARN) of the dataset that is associated with
-- the profile job.
validationConfiguration_rulesetArn :: Lens.Lens' ValidationConfiguration Prelude.Text
validationConfiguration_rulesetArn = Lens.lens (\ValidationConfiguration' {rulesetArn} -> rulesetArn) (\s@ValidationConfiguration' {} a -> s {rulesetArn = a} :: ValidationConfiguration)

instance Data.FromJSON ValidationConfiguration where
  parseJSON =
    Data.withObject
      "ValidationConfiguration"
      ( \x ->
          ValidationConfiguration'
            Prelude.<$> (x Data..:? "ValidationMode")
            Prelude.<*> (x Data..: "RulesetArn")
      )

instance Prelude.Hashable ValidationConfiguration where
  hashWithSalt _salt ValidationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` validationMode
      `Prelude.hashWithSalt` rulesetArn

instance Prelude.NFData ValidationConfiguration where
  rnf ValidationConfiguration' {..} =
    Prelude.rnf validationMode
      `Prelude.seq` Prelude.rnf rulesetArn

instance Data.ToJSON ValidationConfiguration where
  toJSON ValidationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ValidationMode" Data..=)
              Prelude.<$> validationMode,
            Prelude.Just ("RulesetArn" Data..= rulesetArn)
          ]
      )
