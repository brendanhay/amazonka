{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryConfiguration
  ( InventoryConfiguration (..),

    -- * Smart constructor
    mkInventoryConfiguration,

    -- * Lenses
    icDestination,
    icIsEnabled,
    icId,
    icIncludedObjectVersions,
    icSchedule,
    icFilter,
    icOptionalFields,
  )
where

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
  { -- | Contains information about where to publish the inventory results.
    destination :: Types.InventoryDestination,
    -- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
    isEnabled :: Core.Bool,
    -- | The ID used to identify the inventory configuration.
    id :: Types.InventoryId,
    -- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
    includedObjectVersions :: Types.InventoryIncludedObjectVersions,
    -- | Specifies the schedule for generating inventory results.
    schedule :: Types.InventorySchedule,
    -- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
    filter :: Core.Maybe Types.InventoryFilter,
    -- | Contains the optional fields that are included in the inventory results.
    optionalFields :: Core.Maybe [Types.InventoryOptionalField]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryConfiguration' value with any optional fields omitted.
mkInventoryConfiguration ::
  -- | 'destination'
  Types.InventoryDestination ->
  -- | 'isEnabled'
  Core.Bool ->
  -- | 'id'
  Types.InventoryId ->
  -- | 'includedObjectVersions'
  Types.InventoryIncludedObjectVersions ->
  -- | 'schedule'
  Types.InventorySchedule ->
  InventoryConfiguration
mkInventoryConfiguration
  destination
  isEnabled
  id
  includedObjectVersions
  schedule =
    InventoryConfiguration'
      { destination,
        isEnabled,
        id,
        includedObjectVersions,
        schedule,
        filter = Core.Nothing,
        optionalFields = Core.Nothing
      }

-- | Contains information about where to publish the inventory results.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDestination :: Lens.Lens' InventoryConfiguration Types.InventoryDestination
icDestination = Lens.field @"destination"
{-# DEPRECATED icDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIsEnabled :: Lens.Lens' InventoryConfiguration Core.Bool
icIsEnabled = Lens.field @"isEnabled"
{-# DEPRECATED icIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icId :: Lens.Lens' InventoryConfiguration Types.InventoryId
icId = Lens.field @"id"
{-# DEPRECATED icId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- /Note:/ Consider using 'includedObjectVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIncludedObjectVersions :: Lens.Lens' InventoryConfiguration Types.InventoryIncludedObjectVersions
icIncludedObjectVersions = Lens.field @"includedObjectVersions"
{-# DEPRECATED icIncludedObjectVersions "Use generic-lens or generic-optics with 'includedObjectVersions' instead." #-}

-- | Specifies the schedule for generating inventory results.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSchedule :: Lens.Lens' InventoryConfiguration Types.InventorySchedule
icSchedule = Lens.field @"schedule"
{-# DEPRECATED icSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icFilter :: Lens.Lens' InventoryConfiguration (Core.Maybe Types.InventoryFilter)
icFilter = Lens.field @"filter"
{-# DEPRECATED icFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Contains the optional fields that are included in the inventory results.
--
-- /Note:/ Consider using 'optionalFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOptionalFields :: Lens.Lens' InventoryConfiguration (Core.Maybe [Types.InventoryOptionalField])
icOptionalFields = Lens.field @"optionalFields"
{-# DEPRECATED icOptionalFields "Use generic-lens or generic-optics with 'optionalFields' instead." #-}

instance Core.ToXML InventoryConfiguration where
  toXML InventoryConfiguration {..} =
    Core.toXMLNode "Destination" destination
      Core.<> Core.toXMLNode "IsEnabled" isEnabled
      Core.<> Core.toXMLNode "Id" id
      Core.<> Core.toXMLNode "IncludedObjectVersions" includedObjectVersions
      Core.<> Core.toXMLNode "Schedule" schedule
      Core.<> Core.toXMLNode "Filter" Core.<$> filter
      Core.<> Core.toXMLNode
        "OptionalFields"
        (Core.toXMLList "Field" Core.<$> optionalFields)

instance Core.FromXML InventoryConfiguration where
  parseXML x =
    InventoryConfiguration'
      Core.<$> (x Core..@ "Destination")
      Core.<*> (x Core..@ "IsEnabled")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "IncludedObjectVersions")
      Core.<*> (x Core..@ "Schedule")
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@? "OptionalFields" Core..<@> Core.parseXMLList "Field")
