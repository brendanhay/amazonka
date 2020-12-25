{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuota
  ( ResourceQuota (..),

    -- * Smart constructor
    mkResourceQuota,

    -- * Lenses
    rqMaximum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS Elastic Beanstalk quota information for a single resource type in an AWS account. It reflects the resource's limits for this account.
--
-- /See:/ 'mkResourceQuota' smart constructor.
newtype ResourceQuota = ResourceQuota'
  { -- | The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
    maximum :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceQuota' value with any optional fields omitted.
mkResourceQuota ::
  ResourceQuota
mkResourceQuota = ResourceQuota' {maximum = Core.Nothing}

-- | The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqMaximum :: Lens.Lens' ResourceQuota (Core.Maybe Core.Int)
rqMaximum = Lens.field @"maximum"
{-# DEPRECATED rqMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

instance Core.FromXML ResourceQuota where
  parseXML x = ResourceQuota' Core.<$> (x Core..@? "Maximum")
