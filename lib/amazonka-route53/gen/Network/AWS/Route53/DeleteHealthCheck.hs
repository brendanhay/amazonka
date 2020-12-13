{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a health check.
--
-- /Important:/ Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html Replacing and Deleting Health Checks> in the /Amazon Route 53 Developer Guide/ .
-- If you're using AWS Cloud Map and you configured Cloud Map to create a Route 53 health check when you register an instance, you can't use the Route 53 @DeleteHealthCheck@ command to delete the health check. The health check is deleted automatically when you deregister the instance; there can be a delay of several hours before the health check is deleted from Route 53.
module Network.AWS.Route53.DeleteHealthCheck
  ( -- * Creating a request
    DeleteHealthCheck (..),
    mkDeleteHealthCheck,

    -- ** Request lenses
    dhcHealthCheckId,

    -- * Destructuring the response
    DeleteHealthCheckResponse (..),
    mkDeleteHealthCheckResponse,

    -- ** Response lenses
    dhcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | This action deletes a health check.
--
-- /See:/ 'mkDeleteHealthCheck' smart constructor.
newtype DeleteHealthCheck = DeleteHealthCheck'
  { -- | The ID of the health check that you want to delete.
    healthCheckId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHealthCheck' with the minimum fields required to make a request.
--
-- * 'healthCheckId' - The ID of the health check that you want to delete.
mkDeleteHealthCheck ::
  -- | 'healthCheckId'
  Lude.Text ->
  DeleteHealthCheck
mkDeleteHealthCheck pHealthCheckId_ =
  DeleteHealthCheck' {healthCheckId = pHealthCheckId_}

-- | The ID of the health check that you want to delete.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHealthCheckId :: Lens.Lens' DeleteHealthCheck Lude.Text
dhcHealthCheckId = Lens.lens (healthCheckId :: DeleteHealthCheck -> Lude.Text) (\s a -> s {healthCheckId = a} :: DeleteHealthCheck)
{-# DEPRECATED dhcHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

instance Lude.AWSRequest DeleteHealthCheck where
  type Rs DeleteHealthCheck = DeleteHealthCheckResponse
  request = Req.delete route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteHealthCheckResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHealthCheck where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteHealthCheck where
  toPath DeleteHealthCheck' {..} =
    Lude.mconcat
      ["/2013-04-01/healthcheck/", Lude.toBS healthCheckId]

instance Lude.ToQuery DeleteHealthCheck where
  toQuery = Lude.const Lude.mempty

-- | An empty element.
--
-- /See:/ 'mkDeleteHealthCheckResponse' smart constructor.
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHealthCheckResponse
mkDeleteHealthCheckResponse pResponseStatus_ =
  DeleteHealthCheckResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrsResponseStatus :: Lens.Lens' DeleteHealthCheckResponse Lude.Int
dhcrsResponseStatus = Lens.lens (responseStatus :: DeleteHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHealthCheckResponse)
{-# DEPRECATED dhcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
