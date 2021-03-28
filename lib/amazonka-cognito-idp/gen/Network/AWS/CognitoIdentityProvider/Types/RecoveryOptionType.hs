{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
  ( RecoveryOptionType (..)
  -- * Smart constructor
  , mkRecoveryOptionType
  -- * Lenses
  , rotPriority
  , rotName
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A map containing a priority as a key, and recovery method name as a value.
--
-- /See:/ 'mkRecoveryOptionType' smart constructor.
data RecoveryOptionType = RecoveryOptionType'
  { priority :: Core.Natural
    -- ^ A positive integer specifying priority of a method with 1 being the highest priority.
  , name :: Types.RecoveryOptionNameType
    -- ^ Specifies the recovery method for a user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecoveryOptionType' value with any optional fields omitted.
mkRecoveryOptionType
    :: Core.Natural -- ^ 'priority'
    -> Types.RecoveryOptionNameType -- ^ 'name'
    -> RecoveryOptionType
mkRecoveryOptionType priority name
  = RecoveryOptionType'{priority, name}

-- | A positive integer specifying priority of a method with 1 being the highest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rotPriority :: Lens.Lens' RecoveryOptionType Core.Natural
rotPriority = Lens.field @"priority"
{-# INLINEABLE rotPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | Specifies the recovery method for a user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rotName :: Lens.Lens' RecoveryOptionType Types.RecoveryOptionNameType
rotName = Lens.field @"name"
{-# INLINEABLE rotName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RecoveryOptionType where
        toJSON RecoveryOptionType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Priority" Core..= priority),
                  Core.Just ("Name" Core..= name)])

instance Core.FromJSON RecoveryOptionType where
        parseJSON
          = Core.withObject "RecoveryOptionType" Core.$
              \ x ->
                RecoveryOptionType' Core.<$>
                  (x Core..: "Priority") Core.<*> x Core..: "Name"
