-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileTargetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileTargetMapping
  ( SecurityProfileTargetMapping (..),

    -- * Smart constructor
    mkSecurityProfileTargetMapping,

    -- * Lenses
    sptmSecurityProfileIdentifier,
    sptmTarget,
  )
where

import Network.AWS.IoT.Types.SecurityProfileIdentifier
import Network.AWS.IoT.Types.SecurityProfileTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a security profile and the target associated with it.
--
-- /See:/ 'mkSecurityProfileTargetMapping' smart constructor.
data SecurityProfileTargetMapping = SecurityProfileTargetMapping'
  { securityProfileIdentifier ::
      Lude.Maybe
        SecurityProfileIdentifier,
    target ::
      Lude.Maybe SecurityProfileTarget
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityProfileTargetMapping' with the minimum fields required to make a request.
--
-- * 'securityProfileIdentifier' - Information that identifies the security profile.
-- * 'target' - Information about the target (thing group) associated with the security profile.
mkSecurityProfileTargetMapping ::
  SecurityProfileTargetMapping
mkSecurityProfileTargetMapping =
  SecurityProfileTargetMapping'
    { securityProfileIdentifier =
        Lude.Nothing,
      target = Lude.Nothing
    }

-- | Information that identifies the security profile.
--
-- /Note:/ Consider using 'securityProfileIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptmSecurityProfileIdentifier :: Lens.Lens' SecurityProfileTargetMapping (Lude.Maybe SecurityProfileIdentifier)
sptmSecurityProfileIdentifier = Lens.lens (securityProfileIdentifier :: SecurityProfileTargetMapping -> Lude.Maybe SecurityProfileIdentifier) (\s a -> s {securityProfileIdentifier = a} :: SecurityProfileTargetMapping)
{-# DEPRECATED sptmSecurityProfileIdentifier "Use generic-lens or generic-optics with 'securityProfileIdentifier' instead." #-}

-- | Information about the target (thing group) associated with the security profile.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptmTarget :: Lens.Lens' SecurityProfileTargetMapping (Lude.Maybe SecurityProfileTarget)
sptmTarget = Lens.lens (target :: SecurityProfileTargetMapping -> Lude.Maybe SecurityProfileTarget) (\s a -> s {target = a} :: SecurityProfileTargetMapping)
{-# DEPRECATED sptmTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON SecurityProfileTargetMapping where
  parseJSON =
    Lude.withObject
      "SecurityProfileTargetMapping"
      ( \x ->
          SecurityProfileTargetMapping'
            Lude.<$> (x Lude..:? "securityProfileIdentifier")
            Lude.<*> (x Lude..:? "target")
      )
