{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetGroup
  ( TargetGroup (..)
  -- * Smart constructor
  , mkTargetGroup
  -- * Lenses
  , tgArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a load balancer target group.
--
-- /See:/ 'mkTargetGroup' smart constructor.
newtype TargetGroup = TargetGroup'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the target group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroup' value with any optional fields omitted.
mkTargetGroup
    :: TargetGroup
mkTargetGroup = TargetGroup'{arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgArn :: Lens.Lens' TargetGroup (Core.Maybe Core.Text)
tgArn = Lens.field @"arn"
{-# INLINEABLE tgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery TargetGroup where
        toQuery TargetGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Arn") arn

instance Core.FromXML TargetGroup where
        parseXML x = TargetGroup' Core.<$> (x Core..@? "arn")
