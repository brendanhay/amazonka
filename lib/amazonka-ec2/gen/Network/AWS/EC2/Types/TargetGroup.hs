{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroup
  ( TargetGroup (..),

    -- * Smart constructor
    mkTargetGroup,

    -- * Lenses
    tgArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a load balancer target group.
--
-- /See:/ 'mkTargetGroup' smart constructor.
newtype TargetGroup = TargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroup' value with any optional fields omitted.
mkTargetGroup ::
  TargetGroup
mkTargetGroup = TargetGroup' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgArn :: Lens.Lens' TargetGroup (Core.Maybe Types.String)
tgArn = Lens.field @"arn"
{-# DEPRECATED tgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromXML TargetGroup where
  parseXML x = TargetGroup' Core.<$> (x Core..@? "arn")
