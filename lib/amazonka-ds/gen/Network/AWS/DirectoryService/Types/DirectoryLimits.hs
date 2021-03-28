{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryLimits
  ( DirectoryLimits (..)
  -- * Smart constructor
  , mkDirectoryLimits
  -- * Lenses
  , dlCloudOnlyDirectoriesCurrentCount
  , dlCloudOnlyDirectoriesLimit
  , dlCloudOnlyDirectoriesLimitReached
  , dlCloudOnlyMicrosoftADCurrentCount
  , dlCloudOnlyMicrosoftADLimit
  , dlCloudOnlyMicrosoftADLimitReached
  , dlConnectedDirectoriesCurrentCount
  , dlConnectedDirectoriesLimit
  , dlConnectedDirectoriesLimitReached
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains directory limit information for a Region.
--
-- /See:/ 'mkDirectoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { cloudOnlyDirectoriesCurrentCount :: Core.Maybe Core.Natural
    -- ^ The current number of cloud directories in the Region.
  , cloudOnlyDirectoriesLimit :: Core.Maybe Core.Natural
    -- ^ The maximum number of cloud directories allowed in the Region.
  , cloudOnlyDirectoriesLimitReached :: Core.Maybe Core.Bool
    -- ^ Indicates if the cloud directory limit has been reached.
  , cloudOnlyMicrosoftADCurrentCount :: Core.Maybe Core.Natural
    -- ^ The current number of AWS Managed Microsoft AD directories in the region.
  , cloudOnlyMicrosoftADLimit :: Core.Maybe Core.Natural
    -- ^ The maximum number of AWS Managed Microsoft AD directories allowed in the region.
  , cloudOnlyMicrosoftADLimitReached :: Core.Maybe Core.Bool
    -- ^ Indicates if the AWS Managed Microsoft AD directory limit has been reached.
  , connectedDirectoriesCurrentCount :: Core.Maybe Core.Natural
    -- ^ The current number of connected directories in the Region.
  , connectedDirectoriesLimit :: Core.Maybe Core.Natural
    -- ^ The maximum number of connected directories allowed in the Region.
  , connectedDirectoriesLimitReached :: Core.Maybe Core.Bool
    -- ^ Indicates if the connected directory limit has been reached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryLimits' value with any optional fields omitted.
mkDirectoryLimits
    :: DirectoryLimits
mkDirectoryLimits
  = DirectoryLimits'{cloudOnlyDirectoriesCurrentCount = Core.Nothing,
                     cloudOnlyDirectoriesLimit = Core.Nothing,
                     cloudOnlyDirectoriesLimitReached = Core.Nothing,
                     cloudOnlyMicrosoftADCurrentCount = Core.Nothing,
                     cloudOnlyMicrosoftADLimit = Core.Nothing,
                     cloudOnlyMicrosoftADLimitReached = Core.Nothing,
                     connectedDirectoriesCurrentCount = Core.Nothing,
                     connectedDirectoriesLimit = Core.Nothing,
                     connectedDirectoriesLimitReached = Core.Nothing}

-- | The current number of cloud directories in the Region.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlCloudOnlyDirectoriesCurrentCount = Lens.field @"cloudOnlyDirectoriesCurrentCount"
{-# INLINEABLE dlCloudOnlyDirectoriesCurrentCount #-}
{-# DEPRECATED cloudOnlyDirectoriesCurrentCount "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesCurrentCount' instead"  #-}

-- | The maximum number of cloud directories allowed in the Region.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlCloudOnlyDirectoriesLimit = Lens.field @"cloudOnlyDirectoriesLimit"
{-# INLINEABLE dlCloudOnlyDirectoriesLimit #-}
{-# DEPRECATED cloudOnlyDirectoriesLimit "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesLimit' instead"  #-}

-- | Indicates if the cloud directory limit has been reached.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
dlCloudOnlyDirectoriesLimitReached = Lens.field @"cloudOnlyDirectoriesLimitReached"
{-# INLINEABLE dlCloudOnlyDirectoriesLimitReached #-}
{-# DEPRECATED cloudOnlyDirectoriesLimitReached "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesLimitReached' instead"  #-}

-- | The current number of AWS Managed Microsoft AD directories in the region.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlCloudOnlyMicrosoftADCurrentCount = Lens.field @"cloudOnlyMicrosoftADCurrentCount"
{-# INLINEABLE dlCloudOnlyMicrosoftADCurrentCount #-}
{-# DEPRECATED cloudOnlyMicrosoftADCurrentCount "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADCurrentCount' instead"  #-}

-- | The maximum number of AWS Managed Microsoft AD directories allowed in the region.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlCloudOnlyMicrosoftADLimit = Lens.field @"cloudOnlyMicrosoftADLimit"
{-# INLINEABLE dlCloudOnlyMicrosoftADLimit #-}
{-# DEPRECATED cloudOnlyMicrosoftADLimit "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADLimit' instead"  #-}

-- | Indicates if the AWS Managed Microsoft AD directory limit has been reached.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
dlCloudOnlyMicrosoftADLimitReached = Lens.field @"cloudOnlyMicrosoftADLimitReached"
{-# INLINEABLE dlCloudOnlyMicrosoftADLimitReached #-}
{-# DEPRECATED cloudOnlyMicrosoftADLimitReached "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADLimitReached' instead"  #-}

-- | The current number of connected directories in the Region.
--
-- /Note:/ Consider using 'connectedDirectoriesCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlConnectedDirectoriesCurrentCount = Lens.field @"connectedDirectoriesCurrentCount"
{-# INLINEABLE dlConnectedDirectoriesCurrentCount #-}
{-# DEPRECATED connectedDirectoriesCurrentCount "Use generic-lens or generic-optics with 'connectedDirectoriesCurrentCount' instead"  #-}

-- | The maximum number of connected directories allowed in the Region.
--
-- /Note:/ Consider using 'connectedDirectoriesLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesLimit :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Natural)
dlConnectedDirectoriesLimit = Lens.field @"connectedDirectoriesLimit"
{-# INLINEABLE dlConnectedDirectoriesLimit #-}
{-# DEPRECATED connectedDirectoriesLimit "Use generic-lens or generic-optics with 'connectedDirectoriesLimit' instead"  #-}

-- | Indicates if the connected directory limit has been reached.
--
-- /Note:/ Consider using 'connectedDirectoriesLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Core.Maybe Core.Bool)
dlConnectedDirectoriesLimitReached = Lens.field @"connectedDirectoriesLimitReached"
{-# INLINEABLE dlConnectedDirectoriesLimitReached #-}
{-# DEPRECATED connectedDirectoriesLimitReached "Use generic-lens or generic-optics with 'connectedDirectoriesLimitReached' instead"  #-}

instance Core.FromJSON DirectoryLimits where
        parseJSON
          = Core.withObject "DirectoryLimits" Core.$
              \ x ->
                DirectoryLimits' Core.<$>
                  (x Core..:? "CloudOnlyDirectoriesCurrentCount") Core.<*>
                    x Core..:? "CloudOnlyDirectoriesLimit"
                    Core.<*> x Core..:? "CloudOnlyDirectoriesLimitReached"
                    Core.<*> x Core..:? "CloudOnlyMicrosoftADCurrentCount"
                    Core.<*> x Core..:? "CloudOnlyMicrosoftADLimit"
                    Core.<*> x Core..:? "CloudOnlyMicrosoftADLimitReached"
                    Core.<*> x Core..:? "ConnectedDirectoriesCurrentCount"
                    Core.<*> x Core..:? "ConnectedDirectoriesLimit"
                    Core.<*> x Core..:? "ConnectedDirectoriesLimitReached"
