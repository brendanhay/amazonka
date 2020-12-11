{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
module Network.AWS.ECS.DescribeServices
  ( -- * Creating a request
    DescribeServices (..),
    mkDescribeServices,

    -- ** Request lenses
    dInclude,
    dCluster,
    dServices,

    -- * Destructuring the response
    DescribeServicesResponse (..),
    mkDescribeServicesResponse,

    -- ** Response lenses
    dssrsFailures,
    dssrsServices,
    dssrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { include ::
      Lude.Maybe [ServiceField],
    cluster :: Lude.Maybe Lude.Text,
    services :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServices' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the service or services you are describing were launched in any cluster other than the default cluster.
-- * 'include' - Specifies whether you want to see the resource tags for the service. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'services' - A list of services to describe. You may specify up to 10 services to describe in a single operation.
mkDescribeServices ::
  DescribeServices
mkDescribeServices =
  DescribeServices'
    { include = Lude.Nothing,
      cluster = Lude.Nothing,
      services = Lude.mempty
    }

-- | Specifies whether you want to see the resource tags for the service. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInclude :: Lens.Lens' DescribeServices (Lude.Maybe [ServiceField])
dInclude = Lens.lens (include :: DescribeServices -> Lude.Maybe [ServiceField]) (\s a -> s {include = a} :: DescribeServices)
{-# DEPRECATED dInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the service or services you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCluster :: Lens.Lens' DescribeServices (Lude.Maybe Lude.Text)
dCluster = Lens.lens (cluster :: DescribeServices -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DescribeServices)
{-# DEPRECATED dCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | A list of services to describe. You may specify up to 10 services to describe in a single operation.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dServices :: Lens.Lens' DescribeServices [Lude.Text]
dServices = Lens.lens (services :: DescribeServices -> [Lude.Text]) (\s a -> s {services = a} :: DescribeServices)
{-# DEPRECATED dServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.AWSRequest DescribeServices where
  type Rs DescribeServices = DescribeServicesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeServices" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("services" Lude..= services)
          ]
      )

instance Lude.ToPath DescribeServices where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { failures ::
      Lude.Maybe [Failure],
    services :: Lude.Maybe [ContainerService],
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

-- | Creates a value of 'DescribeServicesResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'responseStatus' - The response status code.
-- * 'services' - The list of services described.
mkDescribeServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServicesResponse
mkDescribeServicesResponse pResponseStatus_ =
  DescribeServicesResponse'
    { failures = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsFailures :: Lens.Lens' DescribeServicesResponse (Lude.Maybe [Failure])
dssrsFailures = Lens.lens (failures :: DescribeServicesResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeServicesResponse)
{-# DEPRECATED dssrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of services described.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsServices :: Lens.Lens' DescribeServicesResponse (Lude.Maybe [ContainerService])
dssrsServices = Lens.lens (services :: DescribeServicesResponse -> Lude.Maybe [ContainerService]) (\s a -> s {services = a} :: DescribeServicesResponse)
{-# DEPRECATED dssrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeServicesResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServicesResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
