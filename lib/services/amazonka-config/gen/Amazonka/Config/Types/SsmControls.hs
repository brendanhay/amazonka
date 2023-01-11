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
-- Module      : Amazonka.Config.Types.SsmControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.SsmControls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon Web Services Systems Manager (SSM) specific remediation controls.
--
-- /See:/ 'newSsmControls' smart constructor.
data SsmControls = SsmControls'
  { -- | The maximum percentage of remediation actions allowed to run in parallel
    -- on the non-compliant resources for that specific rule. You can specify a
    -- percentage, such as 10%. The default value is 10.
    concurrentExecutionRatePercentage :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of errors that are allowed before SSM stops running
    -- automations on non-compliant resources for that specific rule. You can
    -- specify a percentage of errors, for example 10%. If you do not specifiy
    -- a percentage, the default is 50%. For example, if you set the
    -- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
    -- running the automations when the fifth error is received.
    errorPercentage :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SsmControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'concurrentExecutionRatePercentage', 'ssmControls_concurrentExecutionRatePercentage' - The maximum percentage of remediation actions allowed to run in parallel
-- on the non-compliant resources for that specific rule. You can specify a
-- percentage, such as 10%. The default value is 10.
--
-- 'errorPercentage', 'ssmControls_errorPercentage' - The percentage of errors that are allowed before SSM stops running
-- automations on non-compliant resources for that specific rule. You can
-- specify a percentage of errors, for example 10%. If you do not specifiy
-- a percentage, the default is 50%. For example, if you set the
-- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
-- running the automations when the fifth error is received.
newSsmControls ::
  SsmControls
newSsmControls =
  SsmControls'
    { concurrentExecutionRatePercentage =
        Prelude.Nothing,
      errorPercentage = Prelude.Nothing
    }

-- | The maximum percentage of remediation actions allowed to run in parallel
-- on the non-compliant resources for that specific rule. You can specify a
-- percentage, such as 10%. The default value is 10.
ssmControls_concurrentExecutionRatePercentage :: Lens.Lens' SsmControls (Prelude.Maybe Prelude.Natural)
ssmControls_concurrentExecutionRatePercentage = Lens.lens (\SsmControls' {concurrentExecutionRatePercentage} -> concurrentExecutionRatePercentage) (\s@SsmControls' {} a -> s {concurrentExecutionRatePercentage = a} :: SsmControls)

-- | The percentage of errors that are allowed before SSM stops running
-- automations on non-compliant resources for that specific rule. You can
-- specify a percentage of errors, for example 10%. If you do not specifiy
-- a percentage, the default is 50%. For example, if you set the
-- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
-- running the automations when the fifth error is received.
ssmControls_errorPercentage :: Lens.Lens' SsmControls (Prelude.Maybe Prelude.Natural)
ssmControls_errorPercentage = Lens.lens (\SsmControls' {errorPercentage} -> errorPercentage) (\s@SsmControls' {} a -> s {errorPercentage = a} :: SsmControls)

instance Data.FromJSON SsmControls where
  parseJSON =
    Data.withObject
      "SsmControls"
      ( \x ->
          SsmControls'
            Prelude.<$> (x Data..:? "ConcurrentExecutionRatePercentage")
            Prelude.<*> (x Data..:? "ErrorPercentage")
      )

instance Prelude.Hashable SsmControls where
  hashWithSalt _salt SsmControls' {..} =
    _salt
      `Prelude.hashWithSalt` concurrentExecutionRatePercentage
      `Prelude.hashWithSalt` errorPercentage

instance Prelude.NFData SsmControls where
  rnf SsmControls' {..} =
    Prelude.rnf concurrentExecutionRatePercentage
      `Prelude.seq` Prelude.rnf errorPercentage

instance Data.ToJSON SsmControls where
  toJSON SsmControls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConcurrentExecutionRatePercentage" Data..=)
              Prelude.<$> concurrentExecutionRatePercentage,
            ("ErrorPercentage" Data..=)
              Prelude.<$> errorPercentage
          ]
      )
