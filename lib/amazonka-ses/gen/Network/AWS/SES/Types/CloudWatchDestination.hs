{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CloudWatchDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.CloudWatchDestination
  ( CloudWatchDestination (..)
  -- * Smart constructor
  , mkCloudWatchDestination
  -- * Lenses
  , cwdDimensionConfigurations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.CloudWatchDimensionConfiguration as Types

-- | Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.
--
-- Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCloudWatchDestination' smart constructor.
newtype CloudWatchDestination = CloudWatchDestination'
  { dimensionConfigurations :: [Types.CloudWatchDimensionConfiguration]
    -- ^ A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchDestination' value with any optional fields omitted.
mkCloudWatchDestination
    :: CloudWatchDestination
mkCloudWatchDestination
  = CloudWatchDestination'{dimensionConfigurations = Core.mempty}

-- | A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
--
-- /Note:/ Consider using 'dimensionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdDimensionConfigurations :: Lens.Lens' CloudWatchDestination [Types.CloudWatchDimensionConfiguration]
cwdDimensionConfigurations = Lens.field @"dimensionConfigurations"
{-# INLINEABLE cwdDimensionConfigurations #-}
{-# DEPRECATED dimensionConfigurations "Use generic-lens or generic-optics with 'dimensionConfigurations' instead"  #-}

instance Core.ToQuery CloudWatchDestination where
        toQuery CloudWatchDestination{..}
          = Core.toQueryPair "DimensionConfigurations"
              (Core.toQueryList "member" dimensionConfigurations)

instance Core.FromXML CloudWatchDestination where
        parseXML x
          = CloudWatchDestination' Core.<$>
              (x Core..@ "DimensionConfigurations" Core..@! Core.mempty Core..<@>
                 Core.parseXMLList "member")
