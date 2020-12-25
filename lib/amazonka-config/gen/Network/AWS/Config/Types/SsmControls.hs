{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.SsmControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SsmControls
  ( SsmControls (..),

    -- * Smart constructor
    mkSsmControls,

    -- * Lenses
    scConcurrentExecutionRatePercentage,
    scErrorPercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | AWS Systems Manager (SSM) specific remediation controls.
--
-- /See:/ 'mkSsmControls' smart constructor.
data SsmControls = SsmControls'
  { -- | The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
    concurrentExecutionRatePercentage :: Core.Maybe Core.Natural,
    -- | The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
    errorPercentage :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SsmControls' value with any optional fields omitted.
mkSsmControls ::
  SsmControls
mkSsmControls =
  SsmControls'
    { concurrentExecutionRatePercentage = Core.Nothing,
      errorPercentage = Core.Nothing
    }

-- | The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
--
-- /Note:/ Consider using 'concurrentExecutionRatePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scConcurrentExecutionRatePercentage :: Lens.Lens' SsmControls (Core.Maybe Core.Natural)
scConcurrentExecutionRatePercentage = Lens.field @"concurrentExecutionRatePercentage"
{-# DEPRECATED scConcurrentExecutionRatePercentage "Use generic-lens or generic-optics with 'concurrentExecutionRatePercentage' instead." #-}

-- | The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
--
-- /Note:/ Consider using 'errorPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scErrorPercentage :: Lens.Lens' SsmControls (Core.Maybe Core.Natural)
scErrorPercentage = Lens.field @"errorPercentage"
{-# DEPRECATED scErrorPercentage "Use generic-lens or generic-optics with 'errorPercentage' instead." #-}

instance Core.FromJSON SsmControls where
  toJSON SsmControls {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConcurrentExecutionRatePercentage" Core..=)
              Core.<$> concurrentExecutionRatePercentage,
            ("ErrorPercentage" Core..=) Core.<$> errorPercentage
          ]
      )

instance Core.FromJSON SsmControls where
  parseJSON =
    Core.withObject "SsmControls" Core.$
      \x ->
        SsmControls'
          Core.<$> (x Core..:? "ConcurrentExecutionRatePercentage")
          Core.<*> (x Core..:? "ErrorPercentage")
