{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.BatchRead
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the read operations in a batch.
module Network.AWS.CloudDirectory.BatchRead
  ( -- * Creating a request
    BatchRead (..),
    mkBatchRead,

    -- ** Request lenses
    brDirectoryARN,
    brConsistencyLevel,
    brOperations,

    -- * Destructuring the response
    BatchReadResponse (..),
    mkBatchReadResponse,

    -- ** Response lenses
    brrsResponses,
    brrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchRead' smart constructor.
data BatchRead = BatchRead'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    -- | A list of operations that are part of the batch.
    operations :: [BatchReadOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchRead' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'operations' - A list of operations that are part of the batch.
mkBatchRead ::
  -- | 'directoryARN'
  Lude.Text ->
  BatchRead
mkBatchRead pDirectoryARN_ =
  BatchRead'
    { directoryARN = pDirectoryARN_,
      consistencyLevel = Lude.Nothing,
      operations = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDirectoryARN :: Lens.Lens' BatchRead Lude.Text
brDirectoryARN = Lens.lens (directoryARN :: BatchRead -> Lude.Text) (\s a -> s {directoryARN = a} :: BatchRead)
{-# DEPRECATED brDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brConsistencyLevel :: Lens.Lens' BatchRead (Lude.Maybe ConsistencyLevel)
brConsistencyLevel = Lens.lens (consistencyLevel :: BatchRead -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: BatchRead)
{-# DEPRECATED brConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | A list of operations that are part of the batch.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brOperations :: Lens.Lens' BatchRead [BatchReadOperation]
brOperations = Lens.lens (operations :: BatchRead -> [BatchReadOperation]) (\s a -> s {operations = a} :: BatchRead)
{-# DEPRECATED brOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

instance Lude.AWSRequest BatchRead where
  type Rs BatchRead = BatchReadResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchReadResponse'
            Lude.<$> (x Lude..?> "Responses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchRead where
  toHeaders BatchRead' {..} =
    Lude.mconcat
      [ "x-amz-data-partition" Lude.=# directoryARN,
        "x-amz-consistency-level" Lude.=# consistencyLevel
      ]

instance Lude.ToJSON BatchRead where
  toJSON BatchRead' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Operations" Lude..= operations)])

instance Lude.ToPath BatchRead where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/batchread"

instance Lude.ToQuery BatchRead where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchReadResponse' smart constructor.
data BatchReadResponse = BatchReadResponse'
  { -- | A list of all the responses for each batch read.
    responses :: Lude.Maybe [BatchReadOperationResponse],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchReadResponse' with the minimum fields required to make a request.
--
-- * 'responses' - A list of all the responses for each batch read.
-- * 'responseStatus' - The response status code.
mkBatchReadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchReadResponse
mkBatchReadResponse pResponseStatus_ =
  BatchReadResponse'
    { responses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of all the responses for each batch read.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrsResponses :: Lens.Lens' BatchReadResponse (Lude.Maybe [BatchReadOperationResponse])
brrsResponses = Lens.lens (responses :: BatchReadResponse -> Lude.Maybe [BatchReadOperationResponse]) (\s a -> s {responses = a} :: BatchReadResponse)
{-# DEPRECATED brrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrsResponseStatus :: Lens.Lens' BatchReadResponse Lude.Int
brrsResponseStatus = Lens.lens (responseStatus :: BatchReadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchReadResponse)
{-# DEPRECATED brrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
