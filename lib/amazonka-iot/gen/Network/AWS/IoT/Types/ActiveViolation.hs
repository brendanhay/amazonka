{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ActiveViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ActiveViolation
  ( ActiveViolation (..),

    -- * Smart constructor
    mkActiveViolation,

    -- * Lenses
    avLastViolationValue,
    avLastViolationTime,
    avViolationStartTime,
    avViolationId,
    avBehavior,
    avSecurityProfileName,
    avThingName,
  )
where

import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.MetricValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an active Device Defender security profile behavior violation.
--
-- /See:/ 'mkActiveViolation' smart constructor.
data ActiveViolation = ActiveViolation'
  { lastViolationValue ::
      Lude.Maybe MetricValue,
    lastViolationTime :: Lude.Maybe Lude.Timestamp,
    violationStartTime :: Lude.Maybe Lude.Timestamp,
    violationId :: Lude.Maybe Lude.Text,
    behavior :: Lude.Maybe Behavior,
    securityProfileName :: Lude.Maybe Lude.Text,
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveViolation' with the minimum fields required to make a request.
--
-- * 'behavior' - The behavior which is being violated.
-- * 'lastViolationTime' - The time the most recent violation occurred.
-- * 'lastViolationValue' - The value of the metric (the measurement) which caused the most recent violation.
-- * 'securityProfileName' - The security profile whose behavior is in violation.
-- * 'thingName' - The name of the thing responsible for the active violation.
-- * 'violationId' - The ID of the active violation.
-- * 'violationStartTime' - The time the violation started.
mkActiveViolation ::
  ActiveViolation
mkActiveViolation =
  ActiveViolation'
    { lastViolationValue = Lude.Nothing,
      lastViolationTime = Lude.Nothing,
      violationStartTime = Lude.Nothing,
      violationId = Lude.Nothing,
      behavior = Lude.Nothing,
      securityProfileName = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The value of the metric (the measurement) which caused the most recent violation.
--
-- /Note:/ Consider using 'lastViolationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avLastViolationValue :: Lens.Lens' ActiveViolation (Lude.Maybe MetricValue)
avLastViolationValue = Lens.lens (lastViolationValue :: ActiveViolation -> Lude.Maybe MetricValue) (\s a -> s {lastViolationValue = a} :: ActiveViolation)
{-# DEPRECATED avLastViolationValue "Use generic-lens or generic-optics with 'lastViolationValue' instead." #-}

-- | The time the most recent violation occurred.
--
-- /Note:/ Consider using 'lastViolationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avLastViolationTime :: Lens.Lens' ActiveViolation (Lude.Maybe Lude.Timestamp)
avLastViolationTime = Lens.lens (lastViolationTime :: ActiveViolation -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastViolationTime = a} :: ActiveViolation)
{-# DEPRECATED avLastViolationTime "Use generic-lens or generic-optics with 'lastViolationTime' instead." #-}

-- | The time the violation started.
--
-- /Note:/ Consider using 'violationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avViolationStartTime :: Lens.Lens' ActiveViolation (Lude.Maybe Lude.Timestamp)
avViolationStartTime = Lens.lens (violationStartTime :: ActiveViolation -> Lude.Maybe Lude.Timestamp) (\s a -> s {violationStartTime = a} :: ActiveViolation)
{-# DEPRECATED avViolationStartTime "Use generic-lens or generic-optics with 'violationStartTime' instead." #-}

-- | The ID of the active violation.
--
-- /Note:/ Consider using 'violationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avViolationId :: Lens.Lens' ActiveViolation (Lude.Maybe Lude.Text)
avViolationId = Lens.lens (violationId :: ActiveViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationId = a} :: ActiveViolation)
{-# DEPRECATED avViolationId "Use generic-lens or generic-optics with 'violationId' instead." #-}

-- | The behavior which is being violated.
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBehavior :: Lens.Lens' ActiveViolation (Lude.Maybe Behavior)
avBehavior = Lens.lens (behavior :: ActiveViolation -> Lude.Maybe Behavior) (\s a -> s {behavior = a} :: ActiveViolation)
{-# DEPRECATED avBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

-- | The security profile whose behavior is in violation.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSecurityProfileName :: Lens.Lens' ActiveViolation (Lude.Maybe Lude.Text)
avSecurityProfileName = Lens.lens (securityProfileName :: ActiveViolation -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: ActiveViolation)
{-# DEPRECATED avSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The name of the thing responsible for the active violation.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avThingName :: Lens.Lens' ActiveViolation (Lude.Maybe Lude.Text)
avThingName = Lens.lens (thingName :: ActiveViolation -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: ActiveViolation)
{-# DEPRECATED avThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.FromJSON ActiveViolation where
  parseJSON =
    Lude.withObject
      "ActiveViolation"
      ( \x ->
          ActiveViolation'
            Lude.<$> (x Lude..:? "lastViolationValue")
            Lude.<*> (x Lude..:? "lastViolationTime")
            Lude.<*> (x Lude..:? "violationStartTime")
            Lude.<*> (x Lude..:? "violationId")
            Lude.<*> (x Lude..:? "behavior")
            Lude.<*> (x Lude..:? "securityProfileName")
            Lude.<*> (x Lude..:? "thingName")
      )
