{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AlarmIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.AlarmIdentifier
  ( AlarmIdentifier (..)
  -- * Smart constructor
  , mkAlarmIdentifier
  -- * Lenses
  , aiRegion
  , aiName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.AlarmName as Types
import qualified Network.AWS.Route53.Types.CloudWatchRegion as Types

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /See:/ 'mkAlarmIdentifier' smart constructor.
data AlarmIdentifier = AlarmIdentifier'
  { region :: Types.CloudWatchRegion
    -- ^ For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in.
--
-- For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
  , name :: Types.AlarmName
    -- ^ The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlarmIdentifier' value with any optional fields omitted.
mkAlarmIdentifier
    :: Types.CloudWatchRegion -- ^ 'region'
    -> Types.AlarmName -- ^ 'name'
    -> AlarmIdentifier
mkAlarmIdentifier region name = AlarmIdentifier'{region, name}

-- | For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in.
--
-- For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiRegion :: Lens.Lens' AlarmIdentifier Types.CloudWatchRegion
aiRegion = Lens.field @"region"
{-# INLINEABLE aiRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiName :: Lens.Lens' AlarmIdentifier Types.AlarmName
aiName = Lens.field @"name"
{-# INLINEABLE aiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToXML AlarmIdentifier where
        toXML AlarmIdentifier{..}
          = Core.toXMLElement "Region" region Core.<>
              Core.toXMLElement "Name" name

instance Core.FromXML AlarmIdentifier where
        parseXML x
          = AlarmIdentifier' Core.<$>
              (x Core..@ "Region") Core.<*> x Core..@ "Name"
