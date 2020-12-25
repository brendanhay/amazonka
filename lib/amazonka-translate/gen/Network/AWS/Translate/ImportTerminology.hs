{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ImportTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a custom terminology, depending on whether or not one already exists for the given terminology name. Importing a terminology with the same name as an existing one will merge the terminologies based on the chosen merge strategy. Currently, the only supported merge strategy is OVERWRITE, and so the imported terminology will overwrite an existing terminology of the same name.
--
-- If you import a terminology that overwrites an existing one, the new terminology take up to 10 minutes to fully propagate and be available for use in a translation due to cache policies with the DataPlane service that performs the translations.
module Network.AWS.Translate.ImportTerminology
  ( -- * Creating a request
    ImportTerminology (..),
    mkImportTerminology,

    -- ** Request lenses
    itName,
    itMergeStrategy,
    itTerminologyData,
    itDescription,
    itEncryptionKey,

    -- * Destructuring the response
    ImportTerminologyResponse (..),
    mkImportTerminologyResponse,

    -- ** Response lenses
    itrrsTerminologyProperties,
    itrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkImportTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { -- | The name of the custom terminology being imported.
    name :: Types.Name,
    -- | The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
    mergeStrategy :: Types.MergeStrategy,
    -- | The terminology data for the custom terminology being imported.
    terminologyData :: Types.TerminologyData,
    -- | The description of the custom terminology being imported.
    description :: Core.Maybe Types.Description,
    -- | The encryption key for the custom terminology being imported.
    encryptionKey :: Core.Maybe Types.EncryptionKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportTerminology' value with any optional fields omitted.
mkImportTerminology ::
  -- | 'name'
  Types.Name ->
  -- | 'mergeStrategy'
  Types.MergeStrategy ->
  -- | 'terminologyData'
  Types.TerminologyData ->
  ImportTerminology
mkImportTerminology name mergeStrategy terminologyData =
  ImportTerminology'
    { name,
      mergeStrategy,
      terminologyData,
      description = Core.Nothing,
      encryptionKey = Core.Nothing
    }

-- | The name of the custom terminology being imported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itName :: Lens.Lens' ImportTerminology Types.Name
itName = Lens.field @"name"
{-# DEPRECATED itName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itMergeStrategy :: Lens.Lens' ImportTerminology Types.MergeStrategy
itMergeStrategy = Lens.field @"mergeStrategy"
{-# DEPRECATED itMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The terminology data for the custom terminology being imported.
--
-- /Note:/ Consider using 'terminologyData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTerminologyData :: Lens.Lens' ImportTerminology Types.TerminologyData
itTerminologyData = Lens.field @"terminologyData"
{-# DEPRECATED itTerminologyData "Use generic-lens or generic-optics with 'terminologyData' instead." #-}

-- | The description of the custom terminology being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDescription :: Lens.Lens' ImportTerminology (Core.Maybe Types.Description)
itDescription = Lens.field @"description"
{-# DEPRECATED itDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The encryption key for the custom terminology being imported.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itEncryptionKey :: Lens.Lens' ImportTerminology (Core.Maybe Types.EncryptionKey)
itEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED itEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

instance Core.FromJSON ImportTerminology where
  toJSON ImportTerminology {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("MergeStrategy" Core..= mergeStrategy),
            Core.Just ("TerminologyData" Core..= terminologyData),
            ("Description" Core..=) Core.<$> description,
            ("EncryptionKey" Core..=) Core.<$> encryptionKey
          ]
      )

instance Core.AWSRequest ImportTerminology where
  type Rs ImportTerminology = ImportTerminologyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.ImportTerminology"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportTerminologyResponse'
            Core.<$> (x Core..:? "TerminologyProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { -- | The properties of the custom terminology being imported.
    terminologyProperties :: Core.Maybe Types.TerminologyProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImportTerminologyResponse' value with any optional fields omitted.
mkImportTerminologyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportTerminologyResponse
mkImportTerminologyResponse responseStatus =
  ImportTerminologyResponse'
    { terminologyProperties = Core.Nothing,
      responseStatus
    }

-- | The properties of the custom terminology being imported.
--
-- /Note:/ Consider using 'terminologyProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrrsTerminologyProperties :: Lens.Lens' ImportTerminologyResponse (Core.Maybe Types.TerminologyProperties)
itrrsTerminologyProperties = Lens.field @"terminologyProperties"
{-# DEPRECATED itrrsTerminologyProperties "Use generic-lens or generic-optics with 'terminologyProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrrsResponseStatus :: Lens.Lens' ImportTerminologyResponse Core.Int
itrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED itrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
