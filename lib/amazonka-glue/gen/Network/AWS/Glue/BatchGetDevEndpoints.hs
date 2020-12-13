{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of development endpoint names. After calling the @ListDevEndpoints@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetDevEndpoints
  ( -- * Creating a request
    BatchGetDevEndpoints (..),
    mkBatchGetDevEndpoints,

    -- ** Request lenses
    bgdeDevEndpointNames,

    -- * Destructuring the response
    BatchGetDevEndpointsResponse (..),
    mkBatchGetDevEndpointsResponse,

    -- ** Response lenses
    bgdersDevEndpointsNotFound,
    bgdersDevEndpoints,
    bgdersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetDevEndpoints' smart constructor.
newtype BatchGetDevEndpoints = BatchGetDevEndpoints'
  { -- | The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
    devEndpointNames :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDevEndpoints' with the minimum fields required to make a request.
--
-- * 'devEndpointNames' - The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
mkBatchGetDevEndpoints ::
  -- | 'devEndpointNames'
  Lude.NonEmpty Lude.Text ->
  BatchGetDevEndpoints
mkBatchGetDevEndpoints pDevEndpointNames_ =
  BatchGetDevEndpoints' {devEndpointNames = pDevEndpointNames_}

-- | The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
--
-- /Note:/ Consider using 'devEndpointNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdeDevEndpointNames :: Lens.Lens' BatchGetDevEndpoints (Lude.NonEmpty Lude.Text)
bgdeDevEndpointNames = Lens.lens (devEndpointNames :: BatchGetDevEndpoints -> Lude.NonEmpty Lude.Text) (\s a -> s {devEndpointNames = a} :: BatchGetDevEndpoints)
{-# DEPRECATED bgdeDevEndpointNames "Use generic-lens or generic-optics with 'devEndpointNames' instead." #-}

instance Lude.AWSRequest BatchGetDevEndpoints where
  type Rs BatchGetDevEndpoints = BatchGetDevEndpointsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetDevEndpointsResponse'
            Lude.<$> (x Lude..?> "DevEndpointsNotFound")
            Lude.<*> (x Lude..?> "DevEndpoints" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetDevEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetDevEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetDevEndpoints where
  toJSON BatchGetDevEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DevEndpointNames" Lude..= devEndpointNames)]
      )

instance Lude.ToPath BatchGetDevEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetDevEndpoints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetDevEndpointsResponse' smart constructor.
data BatchGetDevEndpointsResponse = BatchGetDevEndpointsResponse'
  { -- | A list of @DevEndpoints@ not found.
    devEndpointsNotFound :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Lude.Maybe [DevEndpoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDevEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'devEndpointsNotFound' - A list of @DevEndpoints@ not found.
-- * 'devEndpoints' - A list of @DevEndpoint@ definitions.
-- * 'responseStatus' - The response status code.
mkBatchGetDevEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetDevEndpointsResponse
mkBatchGetDevEndpointsResponse pResponseStatus_ =
  BatchGetDevEndpointsResponse'
    { devEndpointsNotFound =
        Lude.Nothing,
      devEndpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DevEndpoints@ not found.
--
-- /Note:/ Consider using 'devEndpointsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdersDevEndpointsNotFound :: Lens.Lens' BatchGetDevEndpointsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgdersDevEndpointsNotFound = Lens.lens (devEndpointsNotFound :: BatchGetDevEndpointsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {devEndpointsNotFound = a} :: BatchGetDevEndpointsResponse)
{-# DEPRECATED bgdersDevEndpointsNotFound "Use generic-lens or generic-optics with 'devEndpointsNotFound' instead." #-}

-- | A list of @DevEndpoint@ definitions.
--
-- /Note:/ Consider using 'devEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdersDevEndpoints :: Lens.Lens' BatchGetDevEndpointsResponse (Lude.Maybe [DevEndpoint])
bgdersDevEndpoints = Lens.lens (devEndpoints :: BatchGetDevEndpointsResponse -> Lude.Maybe [DevEndpoint]) (\s a -> s {devEndpoints = a} :: BatchGetDevEndpointsResponse)
{-# DEPRECATED bgdersDevEndpoints "Use generic-lens or generic-optics with 'devEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdersResponseStatus :: Lens.Lens' BatchGetDevEndpointsResponse Lude.Int
bgdersResponseStatus = Lens.lens (responseStatus :: BatchGetDevEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetDevEndpointsResponse)
{-# DEPRECATED bgdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
