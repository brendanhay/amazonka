{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.BatchWrite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the write operations in a batch. Either all the operations succeed or none.
module Network.AWS.CloudDirectory.BatchWrite
  ( -- * Creating a request
    BatchWrite (..),
    mkBatchWrite,

    -- ** Request lenses
    bwDirectoryARN,
    bwOperations,

    -- * Destructuring the response
    BatchWriteResponse (..),
    mkBatchWriteResponse,

    -- ** Response lenses
    bwrsResponses,
    bwrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | A list of operations that are part of the batch.
    operations :: [BatchWriteOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWrite' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
-- * 'operations' - A list of operations that are part of the batch.
mkBatchWrite ::
  -- | 'directoryARN'
  Lude.Text ->
  BatchWrite
mkBatchWrite pDirectoryARN_ =
  BatchWrite'
    { directoryARN = pDirectoryARN_,
      operations = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwDirectoryARN :: Lens.Lens' BatchWrite Lude.Text
bwDirectoryARN = Lens.lens (directoryARN :: BatchWrite -> Lude.Text) (\s a -> s {directoryARN = a} :: BatchWrite)
{-# DEPRECATED bwDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwOperations :: Lens.Lens' BatchWrite [BatchWriteOperation]
bwOperations = Lens.lens (operations :: BatchWrite -> [BatchWriteOperation]) (\s a -> s {operations = a} :: BatchWrite)
{-# DEPRECATED bwOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

instance Lude.AWSRequest BatchWrite where
  type Rs BatchWrite = BatchWriteResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchWriteResponse'
            Lude.<$> (x Lude..?> "Responses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchWrite where
  toHeaders BatchWrite' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON BatchWrite where
  toJSON BatchWrite' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Operations" Lude..= operations)])

instance Lude.ToPath BatchWrite where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/batchwrite"

instance Lude.ToQuery BatchWrite where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { -- | A list of all the responses for each batch write.
    responses :: Lude.Maybe [BatchWriteOperationResponse],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchWriteResponse' with the minimum fields required to make a request.
--
-- * 'responses' - A list of all the responses for each batch write.
-- * 'responseStatus' - The response status code.
mkBatchWriteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchWriteResponse
mkBatchWriteResponse pResponseStatus_ =
  BatchWriteResponse'
    { responses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of all the responses for each batch write.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrsResponses :: Lens.Lens' BatchWriteResponse (Lude.Maybe [BatchWriteOperationResponse])
bwrsResponses = Lens.lens (responses :: BatchWriteResponse -> Lude.Maybe [BatchWriteOperationResponse]) (\s a -> s {responses = a} :: BatchWriteResponse)
{-# DEPRECATED bwrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bwrsResponseStatus :: Lens.Lens' BatchWriteResponse Lude.Int
bwrsResponseStatus = Lens.lens (responseStatus :: BatchWriteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchWriteResponse)
{-# DEPRECATED bwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
