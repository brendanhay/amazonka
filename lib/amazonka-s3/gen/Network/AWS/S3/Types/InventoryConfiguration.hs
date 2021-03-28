{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.InventoryConfiguration
  ( InventoryConfiguration (..)
  -- * Smart constructor
  , mkInventoryConfiguration
  -- * Lenses
  , icDestination
  , icIsEnabled
  , icId
  , icIncludedObjectVersions
  , icSchedule
  , icFilter
  , icOptionalFields
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.InventoryDestination as Types
import qualified Network.AWS.S3.Types.InventoryFilter as Types
import qualified Network.AWS.S3.Types.InventoryId as Types
import qualified Network.AWS.S3.Types.InventoryIncludedObjectVersions as Types
import qualified Network.AWS.S3.Types.InventoryOptionalField as Types
import qualified Network.AWS.S3.Types.InventorySchedule as Types

-- | Specifies the inventory configuration for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETInventoryConfig.html GET Bucket inventory> in the /Amazon Simple Storage Service API Reference/ . 
--
-- /See:/ 'mkInventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { destination :: Types.InventoryDestination
    -- ^ Contains information about where to publish the inventory results.
  , isEnabled :: Core.Bool
    -- ^ Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
  , id :: Types.InventoryId
    -- ^ The ID used to identify the inventory configuration.
  , includedObjectVersions :: Types.InventoryIncludedObjectVersions
    -- ^ Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
  , schedule :: Types.InventorySchedule
    -- ^ Specifies the schedule for generating inventory results.
  , filter :: Core.Maybe Types.InventoryFilter
    -- ^ Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
  , optionalFields :: Core.Maybe [Types.InventoryOptionalField]
    -- ^ Contains the optional fields that are included in the inventory results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryConfiguration' value with any optional fields omitted.
mkInventoryConfiguration
    :: Types.InventoryDestination -- ^ 'destination'
    -> Core.Bool -- ^ 'isEnabled'
    -> Types.InventoryId -- ^ 'id'
    -> Types.InventoryIncludedObjectVersions -- ^ 'includedObjectVersions'
    -> Types.InventorySchedule -- ^ 'schedule'
    -> InventoryConfiguration
mkInventoryConfiguration destination isEnabled id
  includedObjectVersions schedule
  = InventoryConfiguration'{destination, isEnabled, id,
                            includedObjectVersions, schedule, filter = Core.Nothing,
                            optionalFields = Core.Nothing}

-- | Contains information about where to publish the inventory results.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDestination :: Lens.Lens' InventoryConfiguration Types.InventoryDestination
icDestination = Lens.field @"destination"
{-# INLINEABLE icDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIsEnabled :: Lens.Lens' InventoryConfiguration Core.Bool
icIsEnabled = Lens.field @"isEnabled"
{-# INLINEABLE icIsEnabled #-}
{-# DEPRECATED isEnabled "Use generic-lens or generic-optics with 'isEnabled' instead"  #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icId :: Lens.Lens' InventoryConfiguration Types.InventoryId
icId = Lens.field @"id"
{-# INLINEABLE icId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- /Note:/ Consider using 'includedObjectVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIncludedObjectVersions :: Lens.Lens' InventoryConfiguration Types.InventoryIncludedObjectVersions
icIncludedObjectVersions = Lens.field @"includedObjectVersions"
{-# INLINEABLE icIncludedObjectVersions #-}
{-# DEPRECATED includedObjectVersions "Use generic-lens or generic-optics with 'includedObjectVersions' instead"  #-}

-- | Specifies the schedule for generating inventory results.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSchedule :: Lens.Lens' InventoryConfiguration Types.InventorySchedule
icSchedule = Lens.field @"schedule"
{-# INLINEABLE icSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icFilter :: Lens.Lens' InventoryConfiguration (Core.Maybe Types.InventoryFilter)
icFilter = Lens.field @"filter"
{-# INLINEABLE icFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Contains the optional fields that are included in the inventory results.
--
-- /Note:/ Consider using 'optionalFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOptionalFields :: Lens.Lens' InventoryConfiguration (Core.Maybe [Types.InventoryOptionalField])
icOptionalFields = Lens.field @"optionalFields"
{-# INLINEABLE icOptionalFields #-}
{-# DEPRECATED optionalFields "Use generic-lens or generic-optics with 'optionalFields' instead"  #-}

instance Core.ToXML InventoryConfiguration where
        toXML InventoryConfiguration{..}
          = Core.toXMLElement "Destination" destination Core.<>
              Core.toXMLElement "IsEnabled" isEnabled
              Core.<> Core.toXMLElement "Id" id
              Core.<>
              Core.toXMLElement "IncludedObjectVersions" includedObjectVersions
              Core.<> Core.toXMLElement "Schedule" schedule
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Filter") filter
              Core.<>
              Core.toXMLElement "OptionalFields"
                (Core.maybe Core.mempty (Core.toXMLList "Field") optionalFields)

instance Core.FromXML InventoryConfiguration where
        parseXML x
          = InventoryConfiguration' Core.<$>
              (x Core..@ "Destination") Core.<*> x Core..@ "IsEnabled" Core.<*>
                x Core..@ "Id"
                Core.<*> x Core..@ "IncludedObjectVersions"
                Core.<*> x Core..@ "Schedule"
                Core.<*> x Core..@? "Filter"
                Core.<*>
                x Core..@? "OptionalFields" Core..<@> Core.parseXMLList "Field"
