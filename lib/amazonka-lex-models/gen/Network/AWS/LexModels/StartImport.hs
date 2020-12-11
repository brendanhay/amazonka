{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    siTags,
    siPayload,
    siResourceType,
    siMergeStrategy,

    -- * Destructuring the response
    StartImportResponse (..),
    mkStartImportResponse,

    -- ** Response lenses
    sirsResourceType,
    sirsImportId,
    sirsCreatedDate,
    sirsName,
    sirsMergeStrategy,
    sirsImportStatus,
    sirsTags,
    sirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartImport' smart constructor.
data StartImport = StartImport'
  { tags :: Lude.Maybe [Tag],
    payload :: Lude.Base64,
    resourceType :: ResourceType,
    mergeStrategy :: MergeStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImport' with the minimum fields required to make a request.
--
-- * 'mergeStrategy' - Specifies the action that the @StartImport@ operation should take when there is an existing resource with the same name.
--
--
--     * FAIL_ON_CONFLICT - The import operation is stopped on the first conflict between a resource in the import file and an existing resource. The name of the resource causing the conflict is in the @failureReason@ field of the response to the @GetImport@ operation.
-- OVERWRITE_LATEST - The import operation proceeds even if there is a conflict with an existing resource. The $LASTEST version of the existing resource is overwritten with the data from the import file.
--
--
-- * 'payload' - A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'resourceType' - Specifies the type of resource to export. Each resource also exports any resources that it depends on.
--
--
--     * A bot exports dependent intents.
--
--
--     * An intent exports dependent slot types.
--
--
-- * 'tags' - A list of tags to add to the imported bot. You can only add tags when you import a bot, you can't add tags to an intent or slot type.
mkStartImport ::
  -- | 'payload'
  Lude.Base64 ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'mergeStrategy'
  MergeStrategy ->
  StartImport
mkStartImport pPayload_ pResourceType_ pMergeStrategy_ =
  StartImport'
    { tags = Lude.Nothing,
      payload = pPayload_,
      resourceType = pResourceType_,
      mergeStrategy = pMergeStrategy_
    }

-- | A list of tags to add to the imported bot. You can only add tags when you import a bot, you can't add tags to an intent or slot type.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTags :: Lens.Lens' StartImport (Lude.Maybe [Tag])
siTags = Lens.lens (tags :: StartImport -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StartImport)
{-# DEPRECATED siTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPayload :: Lens.Lens' StartImport Lude.Base64
siPayload = Lens.lens (payload :: StartImport -> Lude.Base64) (\s a -> s {payload = a} :: StartImport)
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
siResourceType :: Lens.Lens' StartImport ResourceType
siResourceType = Lens.lens (resourceType :: StartImport -> ResourceType) (\s a -> s {resourceType = a} :: StartImport)
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
siMergeStrategy :: Lens.Lens' StartImport MergeStrategy
siMergeStrategy = Lens.lens (mergeStrategy :: StartImport -> MergeStrategy) (\s a -> s {mergeStrategy = a} :: StartImport)
{-# DEPRECATED siMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

instance Lude.AWSRequest StartImport where
  type Rs StartImport = StartImportResponse
  request = Req.postJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartImportResponse'
            Lude.<$> (x Lude..?> "resourceType")
            Lude.<*> (x Lude..?> "importId")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "mergeStrategy")
            Lude.<*> (x Lude..?> "importStatus")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartImport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartImport where
  toJSON StartImport' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("payload" Lude..= payload),
            Lude.Just ("resourceType" Lude..= resourceType),
            Lude.Just ("mergeStrategy" Lude..= mergeStrategy)
          ]
      )

instance Lude.ToPath StartImport where
  toPath = Lude.const "/imports/"

instance Lude.ToQuery StartImport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { resourceType ::
      Lude.Maybe ResourceType,
    importId :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    mergeStrategy :: Lude.Maybe MergeStrategy,
    importStatus :: Lude.Maybe ImportStatus,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImportResponse' with the minimum fields required to make a request.
--
-- * 'createdDate' - A timestamp for the date and time that the import job was requested.
-- * 'importId' - The identifier for the specific import job.
-- * 'importStatus' - The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
-- * 'mergeStrategy' - The action to take when there is a merge conflict.
-- * 'name' - The name given to the import job.
-- * 'resourceType' - The type of resource to import.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A list of tags added to the imported bot.
mkStartImportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartImportResponse
mkStartImportResponse pResponseStatus_ =
  StartImportResponse'
    { resourceType = Lude.Nothing,
      importId = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      mergeStrategy = Lude.Nothing,
      importStatus = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The type of resource to import.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResourceType :: Lens.Lens' StartImportResponse (Lude.Maybe ResourceType)
sirsResourceType = Lens.lens (resourceType :: StartImportResponse -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: StartImportResponse)
{-# DEPRECATED sirsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The identifier for the specific import job.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsImportId :: Lens.Lens' StartImportResponse (Lude.Maybe Lude.Text)
sirsImportId = Lens.lens (importId :: StartImportResponse -> Lude.Maybe Lude.Text) (\s a -> s {importId = a} :: StartImportResponse)
{-# DEPRECATED sirsImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

-- | A timestamp for the date and time that the import job was requested.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsCreatedDate :: Lens.Lens' StartImportResponse (Lude.Maybe Lude.Timestamp)
sirsCreatedDate = Lens.lens (createdDate :: StartImportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: StartImportResponse)
{-# DEPRECATED sirsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name given to the import job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsName :: Lens.Lens' StartImportResponse (Lude.Maybe Lude.Text)
sirsName = Lens.lens (name :: StartImportResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartImportResponse)
{-# DEPRECATED sirsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The action to take when there is a merge conflict.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsMergeStrategy :: Lens.Lens' StartImportResponse (Lude.Maybe MergeStrategy)
sirsMergeStrategy = Lens.lens (mergeStrategy :: StartImportResponse -> Lude.Maybe MergeStrategy) (\s a -> s {mergeStrategy = a} :: StartImportResponse)
{-# DEPRECATED sirsMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsImportStatus :: Lens.Lens' StartImportResponse (Lude.Maybe ImportStatus)
sirsImportStatus = Lens.lens (importStatus :: StartImportResponse -> Lude.Maybe ImportStatus) (\s a -> s {importStatus = a} :: StartImportResponse)
{-# DEPRECATED sirsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | A list of tags added to the imported bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsTags :: Lens.Lens' StartImportResponse (Lude.Maybe [Tag])
sirsTags = Lens.lens (tags :: StartImportResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StartImportResponse)
{-# DEPRECATED sirsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirsResponseStatus :: Lens.Lens' StartImportResponse Lude.Int
sirsResponseStatus = Lens.lens (responseStatus :: StartImportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartImportResponse)
{-# DEPRECATED sirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
