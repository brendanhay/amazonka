{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
  ( SourceTableFeatureDetails (..)
  -- * Smart constructor
  , mkSourceTableFeatureDetails
  -- * Lenses
  , stfdGlobalSecondaryIndexes
  , stfdLocalSecondaryIndexes
  , stfdSSEDescription
  , stfdStreamDescription
  , stfdTimeToLiveDescription
  ) where

import qualified Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo as Types
import qualified Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo as Types
import qualified Network.AWS.DynamoDB.Types.SSEDescription as Types
import qualified Network.AWS.DynamoDB.Types.StreamSpecification as Types
import qualified Network.AWS.DynamoDB.Types.TimeToLiveDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL. 
--
-- /See:/ 'mkSourceTableFeatureDetails' smart constructor.
data SourceTableFeatureDetails = SourceTableFeatureDetails'
  { globalSecondaryIndexes :: Core.Maybe [Types.GlobalSecondaryIndexInfo]
    -- ^ Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection, and ProvisionedThroughput for the GSIs on the table at the time of backup. 
  , localSecondaryIndexes :: Core.Maybe [Types.LocalSecondaryIndexInfo]
    -- ^ Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup. 
  , sSEDescription :: Core.Maybe Types.SSEDescription
    -- ^ The description of the server-side encryption status on the table when the backup was created.
  , streamDescription :: Core.Maybe Types.StreamSpecification
    -- ^ Stream settings on the table when the backup was created.
  , timeToLiveDescription :: Core.Maybe Types.TimeToLiveDescription
    -- ^ Time to Live settings on the table when the backup was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SourceTableFeatureDetails' value with any optional fields omitted.
mkSourceTableFeatureDetails
    :: SourceTableFeatureDetails
mkSourceTableFeatureDetails
  = SourceTableFeatureDetails'{globalSecondaryIndexes = Core.Nothing,
                               localSecondaryIndexes = Core.Nothing,
                               sSEDescription = Core.Nothing, streamDescription = Core.Nothing,
                               timeToLiveDescription = Core.Nothing}

-- | Represents the GSI properties for the table when the backup was created. It includes the IndexName, KeySchema, Projection, and ProvisionedThroughput for the GSIs on the table at the time of backup. 
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdGlobalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe [Types.GlobalSecondaryIndexInfo])
stfdGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE stfdGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | Represents the LSI properties for the table when the backup was created. It includes the IndexName, KeySchema and Projection for the LSIs on the table at the time of backup. 
--
-- /Note:/ Consider using 'localSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdLocalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe [Types.LocalSecondaryIndexInfo])
stfdLocalSecondaryIndexes = Lens.field @"localSecondaryIndexes"
{-# INLINEABLE stfdLocalSecondaryIndexes #-}
{-# DEPRECATED localSecondaryIndexes "Use generic-lens or generic-optics with 'localSecondaryIndexes' instead"  #-}

-- | The description of the server-side encryption status on the table when the backup was created.
--
-- /Note:/ Consider using 'sSEDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdSSEDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe Types.SSEDescription)
stfdSSEDescription = Lens.field @"sSEDescription"
{-# INLINEABLE stfdSSEDescription #-}
{-# DEPRECATED sSEDescription "Use generic-lens or generic-optics with 'sSEDescription' instead"  #-}

-- | Stream settings on the table when the backup was created.
--
-- /Note:/ Consider using 'streamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdStreamDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe Types.StreamSpecification)
stfdStreamDescription = Lens.field @"streamDescription"
{-# INLINEABLE stfdStreamDescription #-}
{-# DEPRECATED streamDescription "Use generic-lens or generic-optics with 'streamDescription' instead"  #-}

-- | Time to Live settings on the table when the backup was created.
--
-- /Note:/ Consider using 'timeToLiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfdTimeToLiveDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe Types.TimeToLiveDescription)
stfdTimeToLiveDescription = Lens.field @"timeToLiveDescription"
{-# INLINEABLE stfdTimeToLiveDescription #-}
{-# DEPRECATED timeToLiveDescription "Use generic-lens or generic-optics with 'timeToLiveDescription' instead"  #-}

instance Core.FromJSON SourceTableFeatureDetails where
        parseJSON
          = Core.withObject "SourceTableFeatureDetails" Core.$
              \ x ->
                SourceTableFeatureDetails' Core.<$>
                  (x Core..:? "GlobalSecondaryIndexes") Core.<*>
                    x Core..:? "LocalSecondaryIndexes"
                    Core.<*> x Core..:? "SSEDescription"
                    Core.<*> x Core..:? "StreamDescription"
                    Core.<*> x Core..:? "TimeToLiveDescription"
