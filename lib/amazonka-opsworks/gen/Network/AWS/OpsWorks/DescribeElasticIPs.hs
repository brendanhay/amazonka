{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeElasticIPs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP addresses> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeElasticIPs
  ( -- * Creating a request
    DescribeElasticIPs (..),
    mkDescribeElasticIPs,

    -- ** Request lenses
    deiInstanceId,
    deiIPs,
    deiStackId,

    -- * Destructuring the response
    DescribeElasticIPsResponse (..),
    mkDescribeElasticIPsResponse,

    -- ** Response lenses
    deirsElasticIPs,
    deirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeElasticIPs' smart constructor.
data DescribeElasticIPs = DescribeElasticIPs'
  { -- | The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
    ips :: Lude.Maybe [Lude.Text],
    -- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticIPs' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
-- * 'ips' - An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
-- * 'stackId' - A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
mkDescribeElasticIPs ::
  DescribeElasticIPs
mkDescribeElasticIPs =
  DescribeElasticIPs'
    { instanceId = Lude.Nothing,
      ips = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiInstanceId :: Lens.Lens' DescribeElasticIPs (Lude.Maybe Lude.Text)
deiInstanceId = Lens.lens (instanceId :: DescribeElasticIPs -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeElasticIPs)
{-# DEPRECATED deiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
--
-- /Note:/ Consider using 'ips' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiIPs :: Lens.Lens' DescribeElasticIPs (Lude.Maybe [Lude.Text])
deiIPs = Lens.lens (ips :: DescribeElasticIPs -> Lude.Maybe [Lude.Text]) (\s a -> s {ips = a} :: DescribeElasticIPs)
{-# DEPRECATED deiIPs "Use generic-lens or generic-optics with 'ips' instead." #-}

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiStackId :: Lens.Lens' DescribeElasticIPs (Lude.Maybe Lude.Text)
deiStackId = Lens.lens (stackId :: DescribeElasticIPs -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeElasticIPs)
{-# DEPRECATED deiStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeElasticIPs where
  type Rs DescribeElasticIPs = DescribeElasticIPsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeElasticIPsResponse'
            Lude.<$> (x Lude..?> "ElasticIps" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticIPs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeElasticIps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeElasticIPs where
  toJSON DescribeElasticIPs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("Ips" Lude..=) Lude.<$> ips,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeElasticIPs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeElasticIPs where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeElasticIps@ request.
--
-- /See:/ 'mkDescribeElasticIPsResponse' smart constructor.
data DescribeElasticIPsResponse = DescribeElasticIPsResponse'
  { -- | An @ElasticIps@ object that describes the specified Elastic IP addresses.
    elasticIPs :: Lude.Maybe [ElasticIP],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticIPsResponse' with the minimum fields required to make a request.
--
-- * 'elasticIPs' - An @ElasticIps@ object that describes the specified Elastic IP addresses.
-- * 'responseStatus' - The response status code.
mkDescribeElasticIPsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticIPsResponse
mkDescribeElasticIPsResponse pResponseStatus_ =
  DescribeElasticIPsResponse'
    { elasticIPs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An @ElasticIps@ object that describes the specified Elastic IP addresses.
--
-- /Note:/ Consider using 'elasticIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deirsElasticIPs :: Lens.Lens' DescribeElasticIPsResponse (Lude.Maybe [ElasticIP])
deirsElasticIPs = Lens.lens (elasticIPs :: DescribeElasticIPsResponse -> Lude.Maybe [ElasticIP]) (\s a -> s {elasticIPs = a} :: DescribeElasticIPsResponse)
{-# DEPRECATED deirsElasticIPs "Use generic-lens or generic-optics with 'elasticIPs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deirsResponseStatus :: Lens.Lens' DescribeElasticIPsResponse Lude.Int
deirsResponseStatus = Lens.lens (responseStatus :: DescribeElasticIPsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticIPsResponse)
{-# DEPRECATED deirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
