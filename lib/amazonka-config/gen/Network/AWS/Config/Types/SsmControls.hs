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
import qualified Network.AWS.Prelude as Lude

-- | AWS Systems Manager (SSM) specific remediation controls.
--
-- /See:/ 'mkSsmControls' smart constructor.
data SsmControls = SsmControls'
  { -- | The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
    concurrentExecutionRatePercentage :: Lude.Maybe Lude.Natural,
    -- | The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
    errorPercentage :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SsmControls' with the minimum fields required to make a request.
--
-- * 'concurrentExecutionRatePercentage' - The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
-- * 'errorPercentage' - The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
mkSsmControls ::
  SsmControls
mkSsmControls =
  SsmControls'
    { concurrentExecutionRatePercentage = Lude.Nothing,
      errorPercentage = Lude.Nothing
    }

-- | The maximum percentage of remediation actions allowed to run in parallel on the non-compliant resources for that specific rule. You can specify a percentage, such as 10%. The default value is 10.
--
-- /Note:/ Consider using 'concurrentExecutionRatePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scConcurrentExecutionRatePercentage :: Lens.Lens' SsmControls (Lude.Maybe Lude.Natural)
scConcurrentExecutionRatePercentage = Lens.lens (concurrentExecutionRatePercentage :: SsmControls -> Lude.Maybe Lude.Natural) (\s a -> s {concurrentExecutionRatePercentage = a} :: SsmControls)
{-# DEPRECATED scConcurrentExecutionRatePercentage "Use generic-lens or generic-optics with 'concurrentExecutionRatePercentage' instead." #-}

-- | The percentage of errors that are allowed before SSM stops running automations on non-compliant resources for that specific rule. You can specify a percentage of errors, for example 10%. If you do not specifiy a percentage, the default is 50%. For example, if you set the ErrorPercentage to 40% for 10 non-compliant resources, then SSM stops running the automations when the fifth error is received.
--
-- /Note:/ Consider using 'errorPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scErrorPercentage :: Lens.Lens' SsmControls (Lude.Maybe Lude.Natural)
scErrorPercentage = Lens.lens (errorPercentage :: SsmControls -> Lude.Maybe Lude.Natural) (\s a -> s {errorPercentage = a} :: SsmControls)
{-# DEPRECATED scErrorPercentage "Use generic-lens or generic-optics with 'errorPercentage' instead." #-}

instance Lude.FromJSON SsmControls where
  parseJSON =
    Lude.withObject
      "SsmControls"
      ( \x ->
          SsmControls'
            Lude.<$> (x Lude..:? "ConcurrentExecutionRatePercentage")
            Lude.<*> (x Lude..:? "ErrorPercentage")
      )

instance Lude.ToJSON SsmControls where
  toJSON SsmControls' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConcurrentExecutionRatePercentage" Lude..=)
              Lude.<$> concurrentExecutionRatePercentage,
            ("ErrorPercentage" Lude..=) Lude.<$> errorPercentage
          ]
      )
