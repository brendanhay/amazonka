{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackProperty
  ( AttackProperty (..)
  -- * Smart constructor
  , mkAttackProperty
  -- * Lenses
  , apAttackLayer
  , apAttackPropertyIdentifier
  , apTopContributors
  , apTotal
  , apUnit
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AttackLayer as Types
import qualified Network.AWS.Shield.Types.AttackPropertyIdentifier as Types
import qualified Network.AWS.Shield.Types.Contributor as Types
import qualified Network.AWS.Shield.Types.Unit as Types

-- | Details of the described attack.
--
-- /See:/ 'mkAttackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { attackLayer :: Core.Maybe Types.AttackLayer
    -- ^ The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
  , attackPropertyIdentifier :: Core.Maybe Types.AttackPropertyIdentifier
    -- ^ Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
  , topContributors :: Core.Maybe [Types.Contributor]
    -- ^ The array of contributor objects that includes the top five contributors to an attack. 
  , total :: Core.Maybe Core.Integer
    -- ^ The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
  , unit :: Core.Maybe Types.Unit
    -- ^ The unit of the @Value@ of the contributions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttackProperty' value with any optional fields omitted.
mkAttackProperty
    :: AttackProperty
mkAttackProperty
  = AttackProperty'{attackLayer = Core.Nothing,
                    attackPropertyIdentifier = Core.Nothing,
                    topContributors = Core.Nothing, total = Core.Nothing,
                    unit = Core.Nothing}

-- | The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- /Note:/ Consider using 'attackLayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttackLayer :: Lens.Lens' AttackProperty (Core.Maybe Types.AttackLayer)
apAttackLayer = Lens.field @"attackLayer"
{-# INLINEABLE apAttackLayer #-}
{-# DEPRECATED attackLayer "Use generic-lens or generic-optics with 'attackLayer' instead"  #-}

-- | Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
--
-- /Note:/ Consider using 'attackPropertyIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttackPropertyIdentifier :: Lens.Lens' AttackProperty (Core.Maybe Types.AttackPropertyIdentifier)
apAttackPropertyIdentifier = Lens.field @"attackPropertyIdentifier"
{-# INLINEABLE apAttackPropertyIdentifier #-}
{-# DEPRECATED attackPropertyIdentifier "Use generic-lens or generic-optics with 'attackPropertyIdentifier' instead"  #-}

-- | The array of contributor objects that includes the top five contributors to an attack. 
--
-- /Note:/ Consider using 'topContributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTopContributors :: Lens.Lens' AttackProperty (Core.Maybe [Types.Contributor])
apTopContributors = Lens.field @"topContributors"
{-# INLINEABLE apTopContributors #-}
{-# DEPRECATED topContributors "Use generic-lens or generic-optics with 'topContributors' instead"  #-}

-- | The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTotal :: Lens.Lens' AttackProperty (Core.Maybe Core.Integer)
apTotal = Lens.field @"total"
{-# INLINEABLE apTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

-- | The unit of the @Value@ of the contributions.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apUnit :: Lens.Lens' AttackProperty (Core.Maybe Types.Unit)
apUnit = Lens.field @"unit"
{-# INLINEABLE apUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON AttackProperty where
        parseJSON
          = Core.withObject "AttackProperty" Core.$
              \ x ->
                AttackProperty' Core.<$>
                  (x Core..:? "AttackLayer") Core.<*>
                    x Core..:? "AttackPropertyIdentifier"
                    Core.<*> x Core..:? "TopContributors"
                    Core.<*> x Core..:? "Total"
                    Core.<*> x Core..:? "Unit"
