{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SecurityProfileTarget
  ( SecurityProfileTarget (..)
  -- * Smart constructor
  , mkSecurityProfileTarget
  -- * Lenses
  , sptArn
  ) where

import qualified Network.AWS.IoT.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A target to which an alert is sent when a security profile behavior is violated.
--
-- /See:/ 'mkSecurityProfileTarget' smart constructor.
newtype SecurityProfileTarget = SecurityProfileTarget'
  { arn :: Types.Arn
    -- ^ The ARN of the security profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityProfileTarget' value with any optional fields omitted.
mkSecurityProfileTarget
    :: Types.Arn -- ^ 'arn'
    -> SecurityProfileTarget
mkSecurityProfileTarget arn = SecurityProfileTarget'{arn}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptArn :: Lens.Lens' SecurityProfileTarget Types.Arn
sptArn = Lens.field @"arn"
{-# INLINEABLE sptArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromJSON SecurityProfileTarget where
        parseJSON
          = Core.withObject "SecurityProfileTarget" Core.$
              \ x -> SecurityProfileTarget' Core.<$> (x Core..: "arn")
