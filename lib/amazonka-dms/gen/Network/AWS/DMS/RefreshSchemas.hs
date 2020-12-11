{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RefreshSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Populates the schema for the specified endpoint. This is an asynchronous operation and can take several minutes. You can check the status of this operation by calling the DescribeRefreshSchemasStatus operation.
module Network.AWS.DMS.RefreshSchemas
  ( -- * Creating a request
    RefreshSchemas (..),
    mkRefreshSchemas,

    -- ** Request lenses
    rsEndpointARN,
    rsReplicationInstanceARN,

    -- * Destructuring the response
    RefreshSchemasResponse (..),
    mkRefreshSchemasResponse,

    -- ** Response lenses
    rsrsRefreshSchemasStatus,
    rsrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRefreshSchemas' smart constructor.
data RefreshSchemas = RefreshSchemas'
  { endpointARN :: Lude.Text,
    replicationInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshSchemas' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
mkRefreshSchemas ::
  -- | 'endpointARN'
  Lude.Text ->
  -- | 'replicationInstanceARN'
  Lude.Text ->
  RefreshSchemas
mkRefreshSchemas pEndpointARN_ pReplicationInstanceARN_ =
  RefreshSchemas'
    { endpointARN = pEndpointARN_,
      replicationInstanceARN = pReplicationInstanceARN_
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEndpointARN :: Lens.Lens' RefreshSchemas Lude.Text
rsEndpointARN = Lens.lens (endpointARN :: RefreshSchemas -> Lude.Text) (\s a -> s {endpointARN = a} :: RefreshSchemas)
{-# DEPRECATED rsEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsReplicationInstanceARN :: Lens.Lens' RefreshSchemas Lude.Text
rsReplicationInstanceARN = Lens.lens (replicationInstanceARN :: RefreshSchemas -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: RefreshSchemas)
{-# DEPRECATED rsReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest RefreshSchemas where
  type Rs RefreshSchemas = RefreshSchemasResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RefreshSchemasResponse'
            Lude.<$> (x Lude..?> "RefreshSchemasStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RefreshSchemas where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.RefreshSchemas" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RefreshSchemas where
  toJSON RefreshSchemas' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EndpointArn" Lude..= endpointARN),
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath RefreshSchemas where
  toPath = Lude.const "/"

instance Lude.ToQuery RefreshSchemas where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkRefreshSchemasResponse' smart constructor.
data RefreshSchemasResponse = RefreshSchemasResponse'
  { refreshSchemasStatus ::
      Lude.Maybe RefreshSchemasStatus,
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

-- | Creates a value of 'RefreshSchemasResponse' with the minimum fields required to make a request.
--
-- * 'refreshSchemasStatus' - The status of the refreshed schema.
-- * 'responseStatus' - The response status code.
mkRefreshSchemasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RefreshSchemasResponse
mkRefreshSchemasResponse pResponseStatus_ =
  RefreshSchemasResponse'
    { refreshSchemasStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the refreshed schema.
--
-- /Note:/ Consider using 'refreshSchemasStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsRefreshSchemasStatus :: Lens.Lens' RefreshSchemasResponse (Lude.Maybe RefreshSchemasStatus)
rsrsRefreshSchemasStatus = Lens.lens (refreshSchemasStatus :: RefreshSchemasResponse -> Lude.Maybe RefreshSchemasStatus) (\s a -> s {refreshSchemasStatus = a} :: RefreshSchemasResponse)
{-# DEPRECATED rsrsRefreshSchemasStatus "Use generic-lens or generic-optics with 'refreshSchemasStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsResponseStatus :: Lens.Lens' RefreshSchemasResponse Lude.Int
rsrsResponseStatus = Lens.lens (responseStatus :: RefreshSchemasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RefreshSchemasResponse)
{-# DEPRECATED rsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
