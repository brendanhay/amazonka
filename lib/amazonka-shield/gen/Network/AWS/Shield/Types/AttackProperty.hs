{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackProperty
  ( AttackProperty (..),

    -- * Smart constructor
    mkAttackProperty,

    -- * Lenses
    apAttackLayer,
    apTopContributors,
    apAttackPropertyIdentifier,
    apTotal,
    apUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackLayer
import Network.AWS.Shield.Types.AttackPropertyIdentifier
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.Unit

-- | Details of the described attack.
--
-- /See:/ 'mkAttackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { attackLayer ::
      Lude.Maybe AttackLayer,
    topContributors :: Lude.Maybe [Contributor],
    attackPropertyIdentifier ::
      Lude.Maybe AttackPropertyIdentifier,
    total :: Lude.Maybe Lude.Integer,
    unit :: Lude.Maybe Unit
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackProperty' with the minimum fields required to make a request.
--
-- * 'attackLayer' - The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
-- * 'attackPropertyIdentifier' - Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
-- * 'topContributors' - The array of contributor objects that includes the top five contributors to an attack.
-- * 'total' - The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
-- * 'unit' - The unit of the @Value@ of the contributions.
mkAttackProperty ::
  AttackProperty
mkAttackProperty =
  AttackProperty'
    { attackLayer = Lude.Nothing,
      topContributors = Lude.Nothing,
      attackPropertyIdentifier = Lude.Nothing,
      total = Lude.Nothing,
      unit = Lude.Nothing
    }

-- | The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- /Note:/ Consider using 'attackLayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttackLayer :: Lens.Lens' AttackProperty (Lude.Maybe AttackLayer)
apAttackLayer = Lens.lens (attackLayer :: AttackProperty -> Lude.Maybe AttackLayer) (\s a -> s {attackLayer = a} :: AttackProperty)
{-# DEPRECATED apAttackLayer "Use generic-lens or generic-optics with 'attackLayer' instead." #-}

-- | The array of contributor objects that includes the top five contributors to an attack.
--
-- /Note:/ Consider using 'topContributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTopContributors :: Lens.Lens' AttackProperty (Lude.Maybe [Contributor])
apTopContributors = Lens.lens (topContributors :: AttackProperty -> Lude.Maybe [Contributor]) (\s a -> s {topContributors = a} :: AttackProperty)
{-# DEPRECATED apTopContributors "Use generic-lens or generic-optics with 'topContributors' instead." #-}

-- | Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
--
-- /Note:/ Consider using 'attackPropertyIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttackPropertyIdentifier :: Lens.Lens' AttackProperty (Lude.Maybe AttackPropertyIdentifier)
apAttackPropertyIdentifier = Lens.lens (attackPropertyIdentifier :: AttackProperty -> Lude.Maybe AttackPropertyIdentifier) (\s a -> s {attackPropertyIdentifier = a} :: AttackProperty)
{-# DEPRECATED apAttackPropertyIdentifier "Use generic-lens or generic-optics with 'attackPropertyIdentifier' instead." #-}

-- | The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTotal :: Lens.Lens' AttackProperty (Lude.Maybe Lude.Integer)
apTotal = Lens.lens (total :: AttackProperty -> Lude.Maybe Lude.Integer) (\s a -> s {total = a} :: AttackProperty)
{-# DEPRECATED apTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The unit of the @Value@ of the contributions.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apUnit :: Lens.Lens' AttackProperty (Lude.Maybe Unit)
apUnit = Lens.lens (unit :: AttackProperty -> Lude.Maybe Unit) (\s a -> s {unit = a} :: AttackProperty)
{-# DEPRECATED apUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON AttackProperty where
  parseJSON =
    Lude.withObject
      "AttackProperty"
      ( \x ->
          AttackProperty'
            Lude.<$> (x Lude..:? "AttackLayer")
            Lude.<*> (x Lude..:? "TopContributors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AttackPropertyIdentifier")
            Lude.<*> (x Lude..:? "Total")
            Lude.<*> (x Lude..:? "Unit")
      )
