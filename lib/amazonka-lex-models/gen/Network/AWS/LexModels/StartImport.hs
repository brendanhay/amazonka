{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.StartImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job to import a resource to Amazon Lex.
module Network.AWS.LexModels.StartImport
  ( -- * Creating a request
    StartImport (..),
    mkStartImport,

    -- ** Request lenses
    siPayload,
    siResourceType,
    siMergeStrategy,
    siTags,

    -- * Destructuring the response
    StartImportResponse (..),
    mkStartImportResponse,

    -- ** Response lenses
    sirrsCreatedDate,
    sirrsImportId,
    sirrsImportStatus,
    sirrsMergeStrategy,
    sirrsName,
    sirrsResourceType,
    sirrsTags,
    sirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImport' smart constructor.
data StartImport = StartImport'
  { -- | A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.
    payload :: Core.Base64,
    -- | Specifies the type of resource to export. Each resource also exports any resources that it depends on.
    --
    --
    --     * A bot exports dependent intents.
    --
    --
    --     * An intent exports dependent slot types.
    resourceType :: Types.ResourceType,
    -- | Specifies the action that the @StartImport@ operation should take when there is an existing resource with the same name.
    --
    --
    --     * FAIL_ON_CONFLICT - The import operation is stopped on the first conflict between a resource in the import file and an existing resource. The name of the resource causing the conflict is in the @failureReason@ field of the response to the @GetImport@ operation.
    -- OVERWRITE_LATEST - The import operation proceeds even if there is a conflict with an existing resource. The $LASTEST version of the existing resource is overwritten with the data from the import file.
    mergeStrategy :: Types.MergeStrategy,
    -- | A list of tags to add to the imported bot. You can only add tags when you import a bot, you can't add tags to an intent or slot type.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImport' value with any optional fields omitted.
mkStartImport ::
  -- | 'payload'
  Core.Base64 ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'mergeStrategy'
  Types.MergeStrategy ->
  StartImport
mkStartImport payload resourceType mergeStrategy =
  StartImport'
    { payload,
      resourceType,
      mergeStrategy,
      tags = Core.Nothing
    }

-- | A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPayload :: Lens.Lens' StartImport Core.Base64
siPayload = Lens.field @"payload"
{-# DEPRECATED siPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | Specifies the type of resource to export. Each resource also exports any resources that it depends on.
--
--
--     * A bot exports dependent intents.
--
--
--     * An intent exports dependent slot types.
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siResourceType :: Lens.Lens' StartImport Types.ResourceType
siResourceType = Lens.field @"resourceType"
{-# DEPRECATED siResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Specifies the action that the @StartImport@ operation should take when there is an existing resource with the same name.
--
--
--     * FAIL_ON_CONFLICT - The import operation is stopped on the first conflict between a resource in the import file and an existing resource. The name of the resource causing the conflict is in the @failureReason@ field of the response to the @GetImport@ operation.
-- OVERWRITE_LATEST - The import operation proceeds even if there is a conflict with an existing resource. The $LASTEST version of the existing resource is overwritten with the data from the import file.
--
--
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMergeStrategy :: Lens.Lens' StartImport Types.MergeStrategy
siMergeStrategy = Lens.field @"mergeStrategy"
{-# DEPRECATED siMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | A list of tags to add to the imported bot. You can only add tags when you import a bot, you can't add tags to an intent or slot type.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTags :: Lens.Lens' StartImport (Core.Maybe [Types.Tag])
siTags = Lens.field @"tags"
{-# DEPRECATED siTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON StartImport where
  toJSON StartImport {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("payload" Core..= payload),
            Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("mergeStrategy" Core..= mergeStrategy),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest StartImport where
  type Rs StartImport = StartImportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/imports/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportResponse'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "importId")
            Core.<*> (x Core..:? "importStatus")
            Core.<*> (x Core..:? "mergeStrategy")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { -- | A timestamp for the date and time that the import job was requested.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier for the specific import job.
    importId :: Core.Maybe Types.String,
    -- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
    importStatus :: Core.Maybe Types.ImportStatus,
    -- | The action to take when there is a merge conflict.
    mergeStrategy :: Core.Maybe Types.MergeStrategy,
    -- | The name given to the import job.
    name :: Core.Maybe Types.Name,
    -- | The type of resource to import.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | A list of tags added to the imported bot.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartImportResponse' value with any optional fields omitted.
mkStartImportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartImportResponse
mkStartImportResponse responseStatus =
  StartImportResponse'
    { createdDate = Core.Nothing,
      importId = Core.Nothing,
      importStatus = Core.Nothing,
      mergeStrategy = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | A timestamp for the date and time that the import job was requested.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsCreatedDate :: Lens.Lens' StartImportResponse (Core.Maybe Core.NominalDiffTime)
sirrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED sirrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The identifier for the specific import job.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsImportId :: Lens.Lens' StartImportResponse (Core.Maybe Types.String)
sirrsImportId = Lens.field @"importId"
{-# DEPRECATED sirrsImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsImportStatus :: Lens.Lens' StartImportResponse (Core.Maybe Types.ImportStatus)
sirrsImportStatus = Lens.field @"importStatus"
{-# DEPRECATED sirrsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | The action to take when there is a merge conflict.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsMergeStrategy :: Lens.Lens' StartImportResponse (Core.Maybe Types.MergeStrategy)
sirrsMergeStrategy = Lens.field @"mergeStrategy"
{-# DEPRECATED sirrsMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The name given to the import job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsName :: Lens.Lens' StartImportResponse (Core.Maybe Types.Name)
sirrsName = Lens.field @"name"
{-# DEPRECATED sirrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of resource to import.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResourceType :: Lens.Lens' StartImportResponse (Core.Maybe Types.ResourceType)
sirrsResourceType = Lens.field @"resourceType"
{-# DEPRECATED sirrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A list of tags added to the imported bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsTags :: Lens.Lens' StartImportResponse (Core.Maybe [Types.Tag])
sirrsTags = Lens.field @"tags"
{-# DEPRECATED sirrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' StartImportResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
