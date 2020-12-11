{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.StopBulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of a bulk deployment. This action returns a status of ''Stopping'' until the deployment is stopped. You cannot start a new bulk deployment while a previous deployment is in the ''Stopping'' state. This action doesn't rollback completed deployments or cancel pending deployments.
module Network.AWS.Greengrass.StopBulkDeployment
  ( -- * Creating a request
    StopBulkDeployment (..),
    mkStopBulkDeployment,

    -- ** Request lenses
    sbdBulkDeploymentId,

    -- * Destructuring the response
    StopBulkDeploymentResponse (..),
    mkStopBulkDeploymentResponse,

    -- ** Response lenses
    sbdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopBulkDeployment' smart constructor.
newtype StopBulkDeployment = StopBulkDeployment'
  { bulkDeploymentId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBulkDeployment' with the minimum fields required to make a request.
--
-- * 'bulkDeploymentId' - The ID of the bulk deployment.
mkStopBulkDeployment ::
  -- | 'bulkDeploymentId'
  Lude.Text ->
  StopBulkDeployment
mkStopBulkDeployment pBulkDeploymentId_ =
  StopBulkDeployment' {bulkDeploymentId = pBulkDeploymentId_}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdBulkDeploymentId :: Lens.Lens' StopBulkDeployment Lude.Text
sbdBulkDeploymentId = Lens.lens (bulkDeploymentId :: StopBulkDeployment -> Lude.Text) (\s a -> s {bulkDeploymentId = a} :: StopBulkDeployment)
{-# DEPRECATED sbdBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

instance Lude.AWSRequest StopBulkDeployment where
  type Rs StopBulkDeployment = StopBulkDeploymentResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopBulkDeploymentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopBulkDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopBulkDeployment where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StopBulkDeployment where
  toPath StopBulkDeployment' {..} =
    Lude.mconcat
      [ "/greengrass/bulk/deployments/",
        Lude.toBS bulkDeploymentId,
        "/$stop"
      ]

instance Lude.ToQuery StopBulkDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopBulkDeploymentResponse' smart constructor.
newtype StopBulkDeploymentResponse = StopBulkDeploymentResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopBulkDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopBulkDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopBulkDeploymentResponse
mkStopBulkDeploymentResponse pResponseStatus_ =
  StopBulkDeploymentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdrsResponseStatus :: Lens.Lens' StopBulkDeploymentResponse Lude.Int
sbdrsResponseStatus = Lens.lens (responseStatus :: StopBulkDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopBulkDeploymentResponse)
{-# DEPRECATED sbdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
