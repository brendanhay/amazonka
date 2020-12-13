{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of health checks that are associated with the current AWS account.
module Network.AWS.Route53.GetHealthCheckCount
  ( -- * Creating a request
    GetHealthCheckCount (..),
    mkGetHealthCheckCount,

    -- * Destructuring the response
    GetHealthCheckCountResponse (..),
    mkGetHealthCheckCountResponse,

    -- ** Response lenses
    ghccrsHealthCheckCount,
    ghccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request for the number of health checks that are associated with the current AWS account.
--
-- /See:/ 'mkGetHealthCheckCount' smart constructor.
data GetHealthCheckCount = GetHealthCheckCount'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckCount' with the minimum fields required to make a request.
mkGetHealthCheckCount ::
  GetHealthCheckCount
mkGetHealthCheckCount = GetHealthCheckCount'

instance Lude.AWSRequest GetHealthCheckCount where
  type Rs GetHealthCheckCount = GetHealthCheckCountResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHealthCheckCountResponse'
            Lude.<$> (x Lude..@ "HealthCheckCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHealthCheckCount where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHealthCheckCount where
  toPath = Lude.const "/2013-04-01/healthcheckcount"

instance Lude.ToQuery GetHealthCheckCount where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @GetHealthCheckCount@ request.
--
-- /See:/ 'mkGetHealthCheckCountResponse' smart constructor.
data GetHealthCheckCountResponse = GetHealthCheckCountResponse'
  { -- | The number of health checks associated with the current AWS account.
    healthCheckCount :: Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHealthCheckCountResponse' with the minimum fields required to make a request.
--
-- * 'healthCheckCount' - The number of health checks associated with the current AWS account.
-- * 'responseStatus' - The response status code.
mkGetHealthCheckCountResponse ::
  -- | 'healthCheckCount'
  Lude.Integer ->
  -- | 'responseStatus'
  Lude.Int ->
  GetHealthCheckCountResponse
mkGetHealthCheckCountResponse pHealthCheckCount_ pResponseStatus_ =
  GetHealthCheckCountResponse'
    { healthCheckCount =
        pHealthCheckCount_,
      responseStatus = pResponseStatus_
    }

-- | The number of health checks associated with the current AWS account.
--
-- /Note:/ Consider using 'healthCheckCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghccrsHealthCheckCount :: Lens.Lens' GetHealthCheckCountResponse Lude.Integer
ghccrsHealthCheckCount = Lens.lens (healthCheckCount :: GetHealthCheckCountResponse -> Lude.Integer) (\s a -> s {healthCheckCount = a} :: GetHealthCheckCountResponse)
{-# DEPRECATED ghccrsHealthCheckCount "Use generic-lens or generic-optics with 'healthCheckCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghccrsResponseStatus :: Lens.Lens' GetHealthCheckCountResponse Lude.Int
ghccrsResponseStatus = Lens.lens (responseStatus :: GetHealthCheckCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHealthCheckCountResponse)
{-# DEPRECATED ghccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
