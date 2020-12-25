{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentResponse
  ( SegmentResponse (..),

    -- * Smart constructor
    mkSegmentResponse,

    -- * Lenses
    srSegmentType,
    srCreationDate,
    srId,
    srArn,
    srApplicationId,
    srDimensions,
    srImportDefinition,
    srLastModifiedDate,
    srName,
    srSegmentGroups,
    srVersion,
    srTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SegmentDimensions as Types
import qualified Network.AWS.Pinpoint.Types.SegmentGroupList as Types
import qualified Network.AWS.Pinpoint.Types.SegmentImportResource as Types
import qualified Network.AWS.Pinpoint.Types.SegmentType as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the configuration, dimension, and other settings for a segment.
--
-- /See:/ 'mkSegmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
  { -- | The segment type. Valid values are:
    --
    --
    --     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.
    --
    --
    --     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
    segmentType :: Types.SegmentType,
    -- | The date and time when the segment was created.
    creationDate :: Core.Text,
    -- | The unique identifier for the segment.
    id :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the segment.
    arn :: Core.Text,
    -- | The unique identifier for the application that the segment is associated with.
    applicationId :: Core.Text,
    -- | The dimension settings for the segment.
    dimensions :: Core.Maybe Types.SegmentDimensions,
    -- | The settings for the import job that's associated with the segment.
    importDefinition :: Core.Maybe Types.SegmentImportResource,
    -- | The date and time when the segment was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The name of the segment.
    name :: Core.Maybe Core.Text,
    -- | A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
    segmentGroups :: Core.Maybe Types.SegmentGroupList,
    -- | The version number of the segment.
    version :: Core.Maybe Core.Int,
    -- | A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentResponse' value with any optional fields omitted.
mkSegmentResponse ::
  -- | 'segmentType'
  Types.SegmentType ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  SegmentResponse
mkSegmentResponse segmentType creationDate id arn applicationId =
  SegmentResponse'
    { segmentType,
      creationDate,
      id,
      arn,
      applicationId,
      dimensions = Core.Nothing,
      importDefinition = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      name = Core.Nothing,
      segmentGroups = Core.Nothing,
      version = Core.Nothing,
      tags = Core.Nothing
    }

-- | The segment type. Valid values are:
--
--
--     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.
--
--
--     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
--
--
--
-- /Note:/ Consider using 'segmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSegmentType :: Lens.Lens' SegmentResponse Types.SegmentType
srSegmentType = Lens.field @"segmentType"
{-# DEPRECATED srSegmentType "Use generic-lens or generic-optics with 'segmentType' instead." #-}

-- | The date and time when the segment was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srCreationDate :: Lens.Lens' SegmentResponse Core.Text
srCreationDate = Lens.field @"creationDate"
{-# DEPRECATED srCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srId :: Lens.Lens' SegmentResponse Core.Text
srId = Lens.field @"id"
{-# DEPRECATED srId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the segment.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srArn :: Lens.Lens' SegmentResponse Core.Text
srArn = Lens.field @"arn"
{-# DEPRECATED srArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier for the application that the segment is associated with.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srApplicationId :: Lens.Lens' SegmentResponse Core.Text
srApplicationId = Lens.field @"applicationId"
{-# DEPRECATED srApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The dimension settings for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDimensions :: Lens.Lens' SegmentResponse (Core.Maybe Types.SegmentDimensions)
srDimensions = Lens.field @"dimensions"
{-# DEPRECATED srDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The settings for the import job that's associated with the segment.
--
-- /Note:/ Consider using 'importDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srImportDefinition :: Lens.Lens' SegmentResponse (Core.Maybe Types.SegmentImportResource)
srImportDefinition = Lens.field @"importDefinition"
{-# DEPRECATED srImportDefinition "Use generic-lens or generic-optics with 'importDefinition' instead." #-}

-- | The date and time when the segment was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srLastModifiedDate :: Lens.Lens' SegmentResponse (Core.Maybe Core.Text)
srLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED srLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name of the segment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' SegmentResponse (Core.Maybe Core.Text)
srName = Lens.field @"name"
{-# DEPRECATED srName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
--
-- /Note:/ Consider using 'segmentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSegmentGroups :: Lens.Lens' SegmentResponse (Core.Maybe Types.SegmentGroupList)
srSegmentGroups = Lens.field @"segmentGroups"
{-# DEPRECATED srSegmentGroups "Use generic-lens or generic-optics with 'segmentGroups' instead." #-}

-- | The version number of the segment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srVersion :: Lens.Lens' SegmentResponse (Core.Maybe Core.Int)
srVersion = Lens.field @"version"
{-# DEPRECATED srVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTags :: Lens.Lens' SegmentResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
srTags = Lens.field @"tags"
{-# DEPRECATED srTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON SegmentResponse where
  parseJSON =
    Core.withObject "SegmentResponse" Core.$
      \x ->
        SegmentResponse'
          Core.<$> (x Core..: "SegmentType")
          Core.<*> (x Core..: "CreationDate")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "Arn")
          Core.<*> (x Core..: "ApplicationId")
          Core.<*> (x Core..:? "Dimensions")
          Core.<*> (x Core..:? "ImportDefinition")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SegmentGroups")
          Core.<*> (x Core..:? "Version")
          Core.<*> (x Core..:? "tags")
