{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.BatchDeleteImportData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more import tasks, each identified by their import ID. Each import task has a number of records that can identify servers or applications.
--
-- AWS Application Discovery Service has built-in matching logic that will identify when discovered servers match existing entries that you've previously discovered, the information for the already-existing discovered server is updated. When you delete an import task that contains records that were used to match, the information in those matched records that comes from the deleted records will also be deleted.
module Network.AWS.Discovery.BatchDeleteImportData
  ( -- * Creating a request
    BatchDeleteImportData (..),
    mkBatchDeleteImportData,

    -- ** Request lenses
    bdidImportTaskIds,

    -- * Destructuring the response
    BatchDeleteImportDataResponse (..),
    mkBatchDeleteImportDataResponse,

    -- ** Response lenses
    bdidrsErrors,
    bdidrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteImportData' smart constructor.
newtype BatchDeleteImportData = BatchDeleteImportData'
  { importTaskIds ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteImportData' with the minimum fields required to make a request.
--
-- * 'importTaskIds' - The IDs for the import tasks that you want to delete.
mkBatchDeleteImportData ::
  -- | 'importTaskIds'
  Lude.NonEmpty Lude.Text ->
  BatchDeleteImportData
mkBatchDeleteImportData pImportTaskIds_ =
  BatchDeleteImportData' {importTaskIds = pImportTaskIds_}

-- | The IDs for the import tasks that you want to delete.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidImportTaskIds :: Lens.Lens' BatchDeleteImportData (Lude.NonEmpty Lude.Text)
bdidImportTaskIds = Lens.lens (importTaskIds :: BatchDeleteImportData -> Lude.NonEmpty Lude.Text) (\s a -> s {importTaskIds = a} :: BatchDeleteImportData)
{-# DEPRECATED bdidImportTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead." #-}

instance Lude.AWSRequest BatchDeleteImportData where
  type Rs BatchDeleteImportData = BatchDeleteImportDataResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteImportDataResponse'
            Lude.<$> (x Lude..?> "errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteImportData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.BatchDeleteImportData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteImportData where
  toJSON BatchDeleteImportData' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("importTaskIds" Lude..= importTaskIds)]
      )

instance Lude.ToPath BatchDeleteImportData where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteImportData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteImportDataResponse' smart constructor.
data BatchDeleteImportDataResponse = BatchDeleteImportDataResponse'
  { errors ::
      Lude.Maybe
        [BatchDeleteImportDataError],
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

-- | Creates a value of 'BatchDeleteImportDataResponse' with the minimum fields required to make a request.
--
-- * 'errors' - Error messages returned for each import task that you deleted as a response for this command.
-- * 'responseStatus' - The response status code.
mkBatchDeleteImportDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteImportDataResponse
mkBatchDeleteImportDataResponse pResponseStatus_ =
  BatchDeleteImportDataResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Error messages returned for each import task that you deleted as a response for this command.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidrsErrors :: Lens.Lens' BatchDeleteImportDataResponse (Lude.Maybe [BatchDeleteImportDataError])
bdidrsErrors = Lens.lens (errors :: BatchDeleteImportDataResponse -> Lude.Maybe [BatchDeleteImportDataError]) (\s a -> s {errors = a} :: BatchDeleteImportDataResponse)
{-# DEPRECATED bdidrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdidrsResponseStatus :: Lens.Lens' BatchDeleteImportDataResponse Lude.Int
bdidrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteImportDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteImportDataResponse)
{-# DEPRECATED bdidrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
