{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.TestGridSession
  ( TestGridSession (..)
  -- * Smart constructor
  , mkTestGridSession
  -- * Lenses
  , tgsArn
  , tgsBillingMinutes
  , tgsCreated
  , tgsEnded
  , tgsSeleniumProperties
  , tgsStatus
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.TestGridSessionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A 'TestGridSession' is a single instance of a browser launched from the URL provided by a call to 'CreateTestGridUrl' .
--
-- /See:/ 'mkTestGridSession' smart constructor.
data TestGridSession = TestGridSession'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the session.
  , billingMinutes :: Core.Maybe Core.Double
    -- ^ The number of billed minutes that were used for this session. 
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the session was started.
  , ended :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the session ended.
  , seleniumProperties :: Core.Maybe Core.Text
    -- ^ A JSON object of options and parameters passed to the Selenium WebDriver.
  , status :: Core.Maybe Types.TestGridSessionStatus
    -- ^ The state of the session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TestGridSession' value with any optional fields omitted.
mkTestGridSession
    :: TestGridSession
mkTestGridSession
  = TestGridSession'{arn = Core.Nothing,
                     billingMinutes = Core.Nothing, created = Core.Nothing,
                     ended = Core.Nothing, seleniumProperties = Core.Nothing,
                     status = Core.Nothing}

-- | The ARN of the session.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsArn :: Lens.Lens' TestGridSession (Core.Maybe Types.Arn)
tgsArn = Lens.field @"arn"
{-# INLINEABLE tgsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The number of billed minutes that were used for this session. 
--
-- /Note:/ Consider using 'billingMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsBillingMinutes :: Lens.Lens' TestGridSession (Core.Maybe Core.Double)
tgsBillingMinutes = Lens.field @"billingMinutes"
{-# INLINEABLE tgsBillingMinutes #-}
{-# DEPRECATED billingMinutes "Use generic-lens or generic-optics with 'billingMinutes' instead"  #-}

-- | The time that the session was started.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsCreated :: Lens.Lens' TestGridSession (Core.Maybe Core.NominalDiffTime)
tgsCreated = Lens.field @"created"
{-# INLINEABLE tgsCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | The time the session ended.
--
-- /Note:/ Consider using 'ended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsEnded :: Lens.Lens' TestGridSession (Core.Maybe Core.NominalDiffTime)
tgsEnded = Lens.field @"ended"
{-# INLINEABLE tgsEnded #-}
{-# DEPRECATED ended "Use generic-lens or generic-optics with 'ended' instead"  #-}

-- | A JSON object of options and parameters passed to the Selenium WebDriver.
--
-- /Note:/ Consider using 'seleniumProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsSeleniumProperties :: Lens.Lens' TestGridSession (Core.Maybe Core.Text)
tgsSeleniumProperties = Lens.field @"seleniumProperties"
{-# INLINEABLE tgsSeleniumProperties #-}
{-# DEPRECATED seleniumProperties "Use generic-lens or generic-optics with 'seleniumProperties' instead"  #-}

-- | The state of the session.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsStatus :: Lens.Lens' TestGridSession (Core.Maybe Types.TestGridSessionStatus)
tgsStatus = Lens.field @"status"
{-# INLINEABLE tgsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON TestGridSession where
        parseJSON
          = Core.withObject "TestGridSession" Core.$
              \ x ->
                TestGridSession' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "billingMinutes" Core.<*>
                    x Core..:? "created"
                    Core.<*> x Core..:? "ended"
                    Core.<*> x Core..:? "seleniumProperties"
                    Core.<*> x Core..:? "status"
