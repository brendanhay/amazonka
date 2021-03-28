{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
  ( ResourceDataSyncDestinationDataSharing (..)
  -- * Smart constructor
  , mkResourceDataSyncDestinationDataSharing
  -- * Lenses
  , rdsddsDestinationDataSharingType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharingType as Types

-- | Synchronize Systems Manager Inventory data from multiple AWS accounts defined in AWS Organizations to a centralized S3 bucket. Data is synchronized to individual key prefixes in the central bucket. Each key prefix represents a different AWS account ID.
--
-- /See:/ 'mkResourceDataSyncDestinationDataSharing' smart constructor.
newtype ResourceDataSyncDestinationDataSharing = ResourceDataSyncDestinationDataSharing'
  { destinationDataSharingType :: Core.Maybe Types.ResourceDataSyncDestinationDataSharingType
    -- ^ The sharing data type. Only @Organization@ is supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDataSyncDestinationDataSharing' value with any optional fields omitted.
mkResourceDataSyncDestinationDataSharing
    :: ResourceDataSyncDestinationDataSharing
mkResourceDataSyncDestinationDataSharing
  = ResourceDataSyncDestinationDataSharing'{destinationDataSharingType
                                              = Core.Nothing}

-- | The sharing data type. Only @Organization@ is supported.
--
-- /Note:/ Consider using 'destinationDataSharingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsddsDestinationDataSharingType :: Lens.Lens' ResourceDataSyncDestinationDataSharing (Core.Maybe Types.ResourceDataSyncDestinationDataSharingType)
rdsddsDestinationDataSharingType = Lens.field @"destinationDataSharingType"
{-# INLINEABLE rdsddsDestinationDataSharingType #-}
{-# DEPRECATED destinationDataSharingType "Use generic-lens or generic-optics with 'destinationDataSharingType' instead"  #-}

instance Core.FromJSON ResourceDataSyncDestinationDataSharing where
        toJSON ResourceDataSyncDestinationDataSharing{..}
          = Core.object
              (Core.catMaybes
                 [("DestinationDataSharingType" Core..=) Core.<$>
                    destinationDataSharingType])

instance Core.FromJSON ResourceDataSyncDestinationDataSharing where
        parseJSON
          = Core.withObject "ResourceDataSyncDestinationDataSharing" Core.$
              \ x ->
                ResourceDataSyncDestinationDataSharing' Core.<$>
                  (x Core..:? "DestinationDataSharingType")
