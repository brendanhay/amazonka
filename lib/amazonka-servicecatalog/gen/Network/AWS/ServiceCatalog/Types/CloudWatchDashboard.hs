{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
  ( CloudWatchDashboard (..)
  -- * Smart constructor
  , mkCloudWatchDashboard
  -- * Lenses
  , cwdName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Name as Types

-- | Information about a CloudWatch dashboard.
--
-- /See:/ 'mkCloudWatchDashboard' smart constructor.
newtype CloudWatchDashboard = CloudWatchDashboard'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the CloudWatch dashboard.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchDashboard' value with any optional fields omitted.
mkCloudWatchDashboard
    :: CloudWatchDashboard
mkCloudWatchDashboard = CloudWatchDashboard'{name = Core.Nothing}

-- | The name of the CloudWatch dashboard.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwdName :: Lens.Lens' CloudWatchDashboard (Core.Maybe Types.Name)
cwdName = Lens.field @"name"
{-# INLINEABLE cwdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON CloudWatchDashboard where
        parseJSON
          = Core.withObject "CloudWatchDashboard" Core.$
              \ x -> CloudWatchDashboard' Core.<$> (x Core..:? "Name")
