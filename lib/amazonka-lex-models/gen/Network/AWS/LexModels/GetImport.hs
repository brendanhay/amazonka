{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an import job started with the @StartImport@ operation.
module Network.AWS.LexModels.GetImport
  ( -- * Creating a request
    GetImport (..),
    mkGetImport,

    -- ** Request lenses
    giImportId,

    -- * Destructuring the response
    GetImportResponse (..),
    mkGetImportResponse,

    -- ** Response lenses
    girsFailureReason,
    girsResourceType,
    girsImportId,
    girsCreatedDate,
    girsName,
    girsMergeStrategy,
    girsImportStatus,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetImport' smart constructor.
newtype GetImport = GetImport' {importId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetImport' with the minimum fields required to make a request.
--
-- * 'importId' - The identifier of the import job information to return.
mkGetImport ::
  -- | 'importId'
  Lude.Text ->
  GetImport
mkGetImport pImportId_ = GetImport' {importId = pImportId_}

-- | The identifier of the import job information to return.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giImportId :: Lens.Lens' GetImport Lude.Text
giImportId = Lens.lens (importId :: GetImport -> Lude.Text) (\s a -> s {importId = a} :: GetImport)
{-# DEPRECATED giImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

instance Lude.AWSRequest GetImport where
  type Rs GetImport = GetImportResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetImportResponse'
            Lude.<$> (x Lude..?> "failureReason" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "resourceType")
            Lude.<*> (x Lude..?> "importId")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "mergeStrategy")
            Lude.<*> (x Lude..?> "importStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetImport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetImport where
  toPath GetImport' {..} =
    Lude.mconcat ["/imports/", Lude.toBS importId]

instance Lude.ToQuery GetImport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { failureReason ::
      Lude.Maybe [Lude.Text],
    resourceType :: Lude.Maybe ResourceType,
    importId :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    mergeStrategy :: Lude.Maybe MergeStrategy,
    importStatus :: Lude.Maybe ImportStatus,
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

-- | Creates a value of 'GetImportResponse' with the minimum fields required to make a request.
--
-- * 'createdDate' - A timestamp for the date and time that the import job was created.
-- * 'failureReason' - A string that describes why an import job failed to complete.
-- * 'importId' - The identifier for the specific import job.
-- * 'importStatus' - The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
-- * 'mergeStrategy' - The action taken when there was a conflict between an existing resource and a resource in the import file.
-- * 'name' - The name given to the import job.
-- * 'resourceType' - The type of resource imported.
-- * 'responseStatus' - The response status code.
mkGetImportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetImportResponse
mkGetImportResponse pResponseStatus_ =
  GetImportResponse'
    { failureReason = Lude.Nothing,
      resourceType = Lude.Nothing,
      importId = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      mergeStrategy = Lude.Nothing,
      importStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that describes why an import job failed to complete.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsFailureReason :: Lens.Lens' GetImportResponse (Lude.Maybe [Lude.Text])
girsFailureReason = Lens.lens (failureReason :: GetImportResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {failureReason = a} :: GetImportResponse)
{-# DEPRECATED girsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The type of resource imported.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResourceType :: Lens.Lens' GetImportResponse (Lude.Maybe ResourceType)
girsResourceType = Lens.lens (resourceType :: GetImportResponse -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: GetImportResponse)
{-# DEPRECATED girsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The identifier for the specific import job.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsImportId :: Lens.Lens' GetImportResponse (Lude.Maybe Lude.Text)
girsImportId = Lens.lens (importId :: GetImportResponse -> Lude.Maybe Lude.Text) (\s a -> s {importId = a} :: GetImportResponse)
{-# DEPRECATED girsImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

-- | A timestamp for the date and time that the import job was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsCreatedDate :: Lens.Lens' GetImportResponse (Lude.Maybe Lude.Timestamp)
girsCreatedDate = Lens.lens (createdDate :: GetImportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetImportResponse)
{-# DEPRECATED girsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name given to the import job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsName :: Lens.Lens' GetImportResponse (Lude.Maybe Lude.Text)
girsName = Lens.lens (name :: GetImportResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetImportResponse)
{-# DEPRECATED girsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The action taken when there was a conflict between an existing resource and a resource in the import file.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsMergeStrategy :: Lens.Lens' GetImportResponse (Lude.Maybe MergeStrategy)
girsMergeStrategy = Lens.lens (mergeStrategy :: GetImportResponse -> Lude.Maybe MergeStrategy) (\s a -> s {mergeStrategy = a} :: GetImportResponse)
{-# DEPRECATED girsMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsImportStatus :: Lens.Lens' GetImportResponse (Lude.Maybe ImportStatus)
girsImportStatus = Lens.lens (importStatus :: GetImportResponse -> Lude.Maybe ImportStatus) (\s a -> s {importStatus = a} :: GetImportResponse)
{-# DEPRECATED girsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetImportResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetImportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetImportResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
