{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSession
  ( TestGridSession (..),

    -- * Smart constructor
    mkTestGridSession,

    -- * Lenses
    tgsStatus,
    tgsArn,
    tgsCreated,
    tgsBillingMinutes,
    tgsEnded,
    tgsSeleniumProperties,
  )
where

import Network.AWS.DeviceFarm.Types.TestGridSessionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A 'TestGridSession' is a single instance of a browser launched from the URL provided by a call to 'CreateTestGridUrl' .
--
-- /See:/ 'mkTestGridSession' smart constructor.
data TestGridSession = TestGridSession'
  { status ::
      Lude.Maybe TestGridSessionStatus,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    billingMinutes :: Lude.Maybe Lude.Double,
    ended :: Lude.Maybe Lude.Timestamp,
    seleniumProperties :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestGridSession' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the session.
-- * 'billingMinutes' - The number of billed minutes that were used for this session.
-- * 'created' - The time that the session was started.
-- * 'ended' - The time the session ended.
-- * 'seleniumProperties' - A JSON object of options and parameters passed to the Selenium WebDriver.
-- * 'status' - The state of the session.
mkTestGridSession ::
  TestGridSession
mkTestGridSession =
  TestGridSession'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      billingMinutes = Lude.Nothing,
      ended = Lude.Nothing,
      seleniumProperties = Lude.Nothing
    }

-- | The state of the session.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsStatus :: Lens.Lens' TestGridSession (Lude.Maybe TestGridSessionStatus)
tgsStatus = Lens.lens (status :: TestGridSession -> Lude.Maybe TestGridSessionStatus) (\s a -> s {status = a} :: TestGridSession)
{-# DEPRECATED tgsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the session.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsArn :: Lens.Lens' TestGridSession (Lude.Maybe Lude.Text)
tgsArn = Lens.lens (arn :: TestGridSession -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TestGridSession)
{-# DEPRECATED tgsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time that the session was started.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsCreated :: Lens.Lens' TestGridSession (Lude.Maybe Lude.Timestamp)
tgsCreated = Lens.lens (created :: TestGridSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: TestGridSession)
{-# DEPRECATED tgsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The number of billed minutes that were used for this session.
--
-- /Note:/ Consider using 'billingMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsBillingMinutes :: Lens.Lens' TestGridSession (Lude.Maybe Lude.Double)
tgsBillingMinutes = Lens.lens (billingMinutes :: TestGridSession -> Lude.Maybe Lude.Double) (\s a -> s {billingMinutes = a} :: TestGridSession)
{-# DEPRECATED tgsBillingMinutes "Use generic-lens or generic-optics with 'billingMinutes' instead." #-}

-- | The time the session ended.
--
-- /Note:/ Consider using 'ended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsEnded :: Lens.Lens' TestGridSession (Lude.Maybe Lude.Timestamp)
tgsEnded = Lens.lens (ended :: TestGridSession -> Lude.Maybe Lude.Timestamp) (\s a -> s {ended = a} :: TestGridSession)
{-# DEPRECATED tgsEnded "Use generic-lens or generic-optics with 'ended' instead." #-}

-- | A JSON object of options and parameters passed to the Selenium WebDriver.
--
-- /Note:/ Consider using 'seleniumProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsSeleniumProperties :: Lens.Lens' TestGridSession (Lude.Maybe Lude.Text)
tgsSeleniumProperties = Lens.lens (seleniumProperties :: TestGridSession -> Lude.Maybe Lude.Text) (\s a -> s {seleniumProperties = a} :: TestGridSession)
{-# DEPRECATED tgsSeleniumProperties "Use generic-lens or generic-optics with 'seleniumProperties' instead." #-}

instance Lude.FromJSON TestGridSession where
  parseJSON =
    Lude.withObject
      "TestGridSession"
      ( \x ->
          TestGridSession'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "billingMinutes")
            Lude.<*> (x Lude..:? "ended")
            Lude.<*> (x Lude..:? "seleniumProperties")
      )
