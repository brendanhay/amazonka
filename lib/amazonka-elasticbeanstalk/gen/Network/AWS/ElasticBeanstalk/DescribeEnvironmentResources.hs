{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resources for this environment.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
  ( -- * Creating a request
    DescribeEnvironmentResources (..),
    mkDescribeEnvironmentResources,

    -- ** Request lenses
    derEnvironmentName,
    derEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentResourcesResponse (..),
    mkDescribeEnvironmentResourcesResponse,

    -- ** Response lenses
    derrsEnvironmentResources,
    derrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe the resources in an environment.
--
-- /See:/ 'mkDescribeEnvironmentResources' smart constructor.
data DescribeEnvironmentResources = DescribeEnvironmentResources'
  { -- | The name of the environment to retrieve AWS resource usage data.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The ID of the environment to retrieve AWS resource usage data.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentResources' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentId' - The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
mkDescribeEnvironmentResources ::
  DescribeEnvironmentResources
mkDescribeEnvironmentResources =
  DescribeEnvironmentResources'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEnvironmentName :: Lens.Lens' DescribeEnvironmentResources (Lude.Maybe Lude.Text)
derEnvironmentName = Lens.lens (environmentName :: DescribeEnvironmentResources -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEnvironmentResources)
{-# DEPRECATED derEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEnvironmentId :: Lens.Lens' DescribeEnvironmentResources (Lude.Maybe Lude.Text)
derEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentResources -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentResources)
{-# DEPRECATED derEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DescribeEnvironmentResources where
  type
    Rs DescribeEnvironmentResources =
      DescribeEnvironmentResourcesResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEnvironmentResourcesResult"
      ( \s h x ->
          DescribeEnvironmentResourcesResponse'
            Lude.<$> (x Lude..@? "EnvironmentResources")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEnvironmentResources where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentResources where
  toQuery DescribeEnvironmentResources' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEnvironmentResources" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | Result message containing a list of environment resource descriptions.
--
-- /See:/ 'mkDescribeEnvironmentResourcesResponse' smart constructor.
data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'
  { -- | A list of 'EnvironmentResourceDescription' .
    environmentResources :: Lude.Maybe EnvironmentResourceDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentResourcesResponse' with the minimum fields required to make a request.
--
-- * 'environmentResources' - A list of 'EnvironmentResourceDescription' .
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentResourcesResponse
mkDescribeEnvironmentResourcesResponse pResponseStatus_ =
  DescribeEnvironmentResourcesResponse'
    { environmentResources =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'EnvironmentResourceDescription' .
--
-- /Note:/ Consider using 'environmentResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEnvironmentResources :: Lens.Lens' DescribeEnvironmentResourcesResponse (Lude.Maybe EnvironmentResourceDescription)
derrsEnvironmentResources = Lens.lens (environmentResources :: DescribeEnvironmentResourcesResponse -> Lude.Maybe EnvironmentResourceDescription) (\s a -> s {environmentResources = a} :: DescribeEnvironmentResourcesResponse)
{-# DEPRECATED derrsEnvironmentResources "Use generic-lens or generic-optics with 'environmentResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEnvironmentResourcesResponse Lude.Int
derrsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentResourcesResponse)
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
