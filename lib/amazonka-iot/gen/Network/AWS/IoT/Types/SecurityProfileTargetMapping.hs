{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileTargetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SecurityProfileTargetMapping
  ( SecurityProfileTargetMapping (..)
  -- * Smart constructor
  , mkSecurityProfileTargetMapping
  -- * Lenses
  , sptmSecurityProfileIdentifier
  , sptmTarget
  ) where

import qualified Network.AWS.IoT.Types.SecurityProfileIdentifier as Types
import qualified Network.AWS.IoT.Types.SecurityProfileTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a security profile and the target associated with it.
--
-- /See:/ 'mkSecurityProfileTargetMapping' smart constructor.
data SecurityProfileTargetMapping = SecurityProfileTargetMapping'
  { securityProfileIdentifier :: Core.Maybe Types.SecurityProfileIdentifier
    -- ^ Information that identifies the security profile.
  , target :: Core.Maybe Types.SecurityProfileTarget
    -- ^ Information about the target (thing group) associated with the security profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityProfileTargetMapping' value with any optional fields omitted.
mkSecurityProfileTargetMapping
    :: SecurityProfileTargetMapping
mkSecurityProfileTargetMapping
  = SecurityProfileTargetMapping'{securityProfileIdentifier =
                                    Core.Nothing,
                                  target = Core.Nothing}

-- | Information that identifies the security profile.
--
-- /Note:/ Consider using 'securityProfileIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptmSecurityProfileIdentifier :: Lens.Lens' SecurityProfileTargetMapping (Core.Maybe Types.SecurityProfileIdentifier)
sptmSecurityProfileIdentifier = Lens.field @"securityProfileIdentifier"
{-# INLINEABLE sptmSecurityProfileIdentifier #-}
{-# DEPRECATED securityProfileIdentifier "Use generic-lens or generic-optics with 'securityProfileIdentifier' instead"  #-}

-- | Information about the target (thing group) associated with the security profile.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptmTarget :: Lens.Lens' SecurityProfileTargetMapping (Core.Maybe Types.SecurityProfileTarget)
sptmTarget = Lens.field @"target"
{-# INLINEABLE sptmTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

instance Core.FromJSON SecurityProfileTargetMapping where
        parseJSON
          = Core.withObject "SecurityProfileTargetMapping" Core.$
              \ x ->
                SecurityProfileTargetMapping' Core.<$>
                  (x Core..:? "securityProfileIdentifier") Core.<*>
                    x Core..:? "target"
