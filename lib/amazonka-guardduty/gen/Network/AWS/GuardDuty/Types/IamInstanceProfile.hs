{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IamInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.IamInstanceProfile
  ( IamInstanceProfile (..)
  -- * Smart constructor
  , mkIamInstanceProfile
  -- * Lenses
  , iipArn
  , iipId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the EC2 instance profile.
--
-- /See:/ 'mkIamInstanceProfile' smart constructor.
data IamInstanceProfile = IamInstanceProfile'
  { arn :: Core.Maybe Core.Text
    -- ^ The profile ARN of the EC2 instance.
  , id :: Core.Maybe Core.Text
    -- ^ The profile ID of the EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IamInstanceProfile' value with any optional fields omitted.
mkIamInstanceProfile
    :: IamInstanceProfile
mkIamInstanceProfile
  = IamInstanceProfile'{arn = Core.Nothing, id = Core.Nothing}

-- | The profile ARN of the EC2 instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipArn :: Lens.Lens' IamInstanceProfile (Core.Maybe Core.Text)
iipArn = Lens.field @"arn"
{-# INLINEABLE iipArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The profile ID of the EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipId :: Lens.Lens' IamInstanceProfile (Core.Maybe Core.Text)
iipId = Lens.field @"id"
{-# INLINEABLE iipId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON IamInstanceProfile where
        parseJSON
          = Core.withObject "IamInstanceProfile" Core.$
              \ x ->
                IamInstanceProfile' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "id"
