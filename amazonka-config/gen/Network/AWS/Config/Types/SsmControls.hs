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
-- Module      : Network.AWS.Config.Types.SsmControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SsmControls where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | AWS Systems Manager (SSM) specific remediation controls.
--
-- /See:/ 'newSsmControls' smart constructor.
data SsmControls = SsmControls'
  { -- | The percentage of errors that are allowed before SSM stops running
    -- automations on non-compliant resources for that specific rule. You can
    -- specify a percentage of errors, for example 10%. If you do not specifiy
    -- a percentage, the default is 50%. For example, if you set the
    -- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
    -- running the automations when the fifth error is received.
    errorPercentage :: Core.Maybe Core.Natural,
    -- | The maximum percentage of remediation actions allowed to run in parallel
    -- on the non-compliant resources for that specific rule. You can specify a
    -- percentage, such as 10%. The default value is 10.
    concurrentExecutionRatePercentage :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SsmControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorPercentage', 'ssmControls_errorPercentage' - The percentage of errors that are allowed before SSM stops running
-- automations on non-compliant resources for that specific rule. You can
-- specify a percentage of errors, for example 10%. If you do not specifiy
-- a percentage, the default is 50%. For example, if you set the
-- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
-- running the automations when the fifth error is received.
--
-- 'concurrentExecutionRatePercentage', 'ssmControls_concurrentExecutionRatePercentage' - The maximum percentage of remediation actions allowed to run in parallel
-- on the non-compliant resources for that specific rule. You can specify a
-- percentage, such as 10%. The default value is 10.
newSsmControls ::
  SsmControls
newSsmControls =
  SsmControls'
    { errorPercentage = Core.Nothing,
      concurrentExecutionRatePercentage = Core.Nothing
    }

-- | The percentage of errors that are allowed before SSM stops running
-- automations on non-compliant resources for that specific rule. You can
-- specify a percentage of errors, for example 10%. If you do not specifiy
-- a percentage, the default is 50%. For example, if you set the
-- ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops
-- running the automations when the fifth error is received.
ssmControls_errorPercentage :: Lens.Lens' SsmControls (Core.Maybe Core.Natural)
ssmControls_errorPercentage = Lens.lens (\SsmControls' {errorPercentage} -> errorPercentage) (\s@SsmControls' {} a -> s {errorPercentage = a} :: SsmControls)

-- | The maximum percentage of remediation actions allowed to run in parallel
-- on the non-compliant resources for that specific rule. You can specify a
-- percentage, such as 10%. The default value is 10.
ssmControls_concurrentExecutionRatePercentage :: Lens.Lens' SsmControls (Core.Maybe Core.Natural)
ssmControls_concurrentExecutionRatePercentage = Lens.lens (\SsmControls' {concurrentExecutionRatePercentage} -> concurrentExecutionRatePercentage) (\s@SsmControls' {} a -> s {concurrentExecutionRatePercentage = a} :: SsmControls)

instance Core.FromJSON SsmControls where
  parseJSON =
    Core.withObject
      "SsmControls"
      ( \x ->
          SsmControls'
            Core.<$> (x Core..:? "ErrorPercentage")
            Core.<*> (x Core..:? "ConcurrentExecutionRatePercentage")
      )

instance Core.Hashable SsmControls

instance Core.NFData SsmControls

instance Core.ToJSON SsmControls where
  toJSON SsmControls' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ErrorPercentage" Core..=)
              Core.<$> errorPercentage,
            ("ConcurrentExecutionRatePercentage" Core..=)
              Core.<$> concurrentExecutionRatePercentage
          ]
      )
