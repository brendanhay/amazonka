{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchDeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of connection definitions from the Data Catalog.
module Network.AWS.Glue.BatchDeleteConnection
  ( -- * Creating a request
    BatchDeleteConnection (..),
    mkBatchDeleteConnection,

    -- ** Request lenses
    bdcConnectionNameList,
    bdcCatalogId,

    -- * Destructuring the response
    BatchDeleteConnectionResponse (..),
    mkBatchDeleteConnectionResponse,

    -- ** Response lenses
    bdcrsSucceeded,
    bdcrsErrors,
    bdcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteConnection' smart constructor.
data BatchDeleteConnection = BatchDeleteConnection'
  { -- | A list of names of the connections to delete.
    connectionNameList :: [Lude.Text],
    -- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteConnection' with the minimum fields required to make a request.
--
-- * 'connectionNameList' - A list of names of the connections to delete.
-- * 'catalogId' - The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
mkBatchDeleteConnection ::
  BatchDeleteConnection
mkBatchDeleteConnection =
  BatchDeleteConnection'
    { connectionNameList = Lude.mempty,
      catalogId = Lude.Nothing
    }

-- | A list of names of the connections to delete.
--
-- /Note:/ Consider using 'connectionNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcConnectionNameList :: Lens.Lens' BatchDeleteConnection [Lude.Text]
bdcConnectionNameList = Lens.lens (connectionNameList :: BatchDeleteConnection -> [Lude.Text]) (\s a -> s {connectionNameList = a} :: BatchDeleteConnection)
{-# DEPRECATED bdcConnectionNameList "Use generic-lens or generic-optics with 'connectionNameList' instead." #-}

-- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcCatalogId :: Lens.Lens' BatchDeleteConnection (Lude.Maybe Lude.Text)
bdcCatalogId = Lens.lens (catalogId :: BatchDeleteConnection -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: BatchDeleteConnection)
{-# DEPRECATED bdcCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

instance Lude.AWSRequest BatchDeleteConnection where
  type Rs BatchDeleteConnection = BatchDeleteConnectionResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteConnectionResponse'
            Lude.<$> (x Lude..?> "Succeeded" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchDeleteConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteConnection where
  toJSON BatchDeleteConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConnectionNameList" Lude..= connectionNameList),
            ("CatalogId" Lude..=) Lude.<$> catalogId
          ]
      )

instance Lude.ToPath BatchDeleteConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteConnectionResponse' smart constructor.
data BatchDeleteConnectionResponse = BatchDeleteConnectionResponse'
  { -- | A list of names of the connection definitions that were successfully deleted.
    succeeded :: Lude.Maybe [Lude.Text],
    -- | A map of the names of connections that were not successfully deleted to error details.
    errors :: Lude.Maybe (Lude.HashMap Lude.Text (ErrorDetail)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteConnectionResponse' with the minimum fields required to make a request.
--
-- * 'succeeded' - A list of names of the connection definitions that were successfully deleted.
-- * 'errors' - A map of the names of connections that were not successfully deleted to error details.
-- * 'responseStatus' - The response status code.
mkBatchDeleteConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteConnectionResponse
mkBatchDeleteConnectionResponse pResponseStatus_ =
  BatchDeleteConnectionResponse'
    { succeeded = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of names of the connection definitions that were successfully deleted.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrsSucceeded :: Lens.Lens' BatchDeleteConnectionResponse (Lude.Maybe [Lude.Text])
bdcrsSucceeded = Lens.lens (succeeded :: BatchDeleteConnectionResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {succeeded = a} :: BatchDeleteConnectionResponse)
{-# DEPRECATED bdcrsSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | A map of the names of connections that were not successfully deleted to error details.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrsErrors :: Lens.Lens' BatchDeleteConnectionResponse (Lude.Maybe (Lude.HashMap Lude.Text (ErrorDetail)))
bdcrsErrors = Lens.lens (errors :: BatchDeleteConnectionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (ErrorDetail))) (\s a -> s {errors = a} :: BatchDeleteConnectionResponse)
{-# DEPRECATED bdcrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcrsResponseStatus :: Lens.Lens' BatchDeleteConnectionResponse Lude.Int
bdcrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteConnectionResponse)
{-# DEPRECATED bdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
