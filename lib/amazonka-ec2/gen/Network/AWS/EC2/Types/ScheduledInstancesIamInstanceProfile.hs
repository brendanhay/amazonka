{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile
  ( ScheduledInstancesIamInstanceProfile (..)
  -- * Smart constructor
  , mkScheduledInstancesIamInstanceProfile
  -- * Lenses
  , siiipArn
  , siiipName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IAM instance profile for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesIamInstanceProfile' smart constructor.
data ScheduledInstancesIamInstanceProfile = ScheduledInstancesIamInstanceProfile'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN).
  , name :: Core.Maybe Core.Text
    -- ^ The name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesIamInstanceProfile' value with any optional fields omitted.
mkScheduledInstancesIamInstanceProfile
    :: ScheduledInstancesIamInstanceProfile
mkScheduledInstancesIamInstanceProfile
  = ScheduledInstancesIamInstanceProfile'{arn = Core.Nothing,
                                          name = Core.Nothing}

-- | The Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiipArn :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Core.Maybe Core.Text)
siiipArn = Lens.field @"arn"
{-# INLINEABLE siiipArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiipName :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Core.Maybe Core.Text)
siiipName = Lens.field @"name"
{-# INLINEABLE siiipName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery ScheduledInstancesIamInstanceProfile where
        toQuery ScheduledInstancesIamInstanceProfile{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Arn") arn Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Name") name
