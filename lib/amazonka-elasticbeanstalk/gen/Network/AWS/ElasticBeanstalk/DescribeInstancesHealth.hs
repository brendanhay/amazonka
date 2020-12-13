{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html enhanced health reporting> .
module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
  ( -- * Creating a request
    DescribeInstancesHealth (..),
    mkDescribeInstancesHealth,

    -- ** Request lenses
    dihNextToken,
    dihEnvironmentName,
    dihAttributeNames,
    dihEnvironmentId,

    -- * Destructuring the response
    DescribeInstancesHealthResponse (..),
    mkDescribeInstancesHealthResponse,

    -- ** Response lenses
    dihrsInstanceHealthList,
    dihrsNextToken,
    dihrsRefreshedAt,
    dihrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Parameters for a call to @DescribeInstancesHealth@ .
--
-- /See:/ 'mkDescribeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
  { -- | Specify the pagination token returned by a previous call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specify the AWS Elastic Beanstalk environment by name.
    environmentName :: Lude.Maybe Lude.Text,
    -- | Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
    attributeNames :: Lude.Maybe [InstancesHealthAttribute],
    -- | Specify the AWS Elastic Beanstalk environment by ID.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancesHealth' with the minimum fields required to make a request.
--
-- * 'nextToken' - Specify the pagination token returned by a previous call.
-- * 'environmentName' - Specify the AWS Elastic Beanstalk environment by name.
-- * 'attributeNames' - Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
-- * 'environmentId' - Specify the AWS Elastic Beanstalk environment by ID.
mkDescribeInstancesHealth ::
  DescribeInstancesHealth
mkDescribeInstancesHealth =
  DescribeInstancesHealth'
    { nextToken = Lude.Nothing,
      environmentName = Lude.Nothing,
      attributeNames = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | Specify the pagination token returned by a previous call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihNextToken :: Lens.Lens' DescribeInstancesHealth (Lude.Maybe Lude.Text)
dihNextToken = Lens.lens (nextToken :: DescribeInstancesHealth -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancesHealth)
{-# DEPRECATED dihNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify the AWS Elastic Beanstalk environment by name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihEnvironmentName :: Lens.Lens' DescribeInstancesHealth (Lude.Maybe Lude.Text)
dihEnvironmentName = Lens.lens (environmentName :: DescribeInstancesHealth -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeInstancesHealth)
{-# DEPRECATED dihEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihAttributeNames :: Lens.Lens' DescribeInstancesHealth (Lude.Maybe [InstancesHealthAttribute])
dihAttributeNames = Lens.lens (attributeNames :: DescribeInstancesHealth -> Lude.Maybe [InstancesHealthAttribute]) (\s a -> s {attributeNames = a} :: DescribeInstancesHealth)
{-# DEPRECATED dihAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | Specify the AWS Elastic Beanstalk environment by ID.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihEnvironmentId :: Lens.Lens' DescribeInstancesHealth (Lude.Maybe Lude.Text)
dihEnvironmentId = Lens.lens (environmentId :: DescribeInstancesHealth -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeInstancesHealth)
{-# DEPRECATED dihEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DescribeInstancesHealth where
  type Rs DescribeInstancesHealth = DescribeInstancesHealthResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeInstancesHealthResult"
      ( \s h x ->
          DescribeInstancesHealthResponse'
            Lude.<$> ( x Lude..@? "InstanceHealthList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (x Lude..@? "RefreshedAt")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstancesHealth where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstancesHealth where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstancesHealth where
  toQuery DescribeInstancesHealth' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstancesHealth" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "EnvironmentName" Lude.=: environmentName,
        "AttributeNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> attributeNames),
        "EnvironmentId" Lude.=: environmentId
      ]

-- | Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkDescribeInstancesHealthResponse' smart constructor.
data DescribeInstancesHealthResponse = DescribeInstancesHealthResponse'
  { -- | Detailed health information about each instance.
    --
    -- The output differs slightly between Linux and Windows environments. There is a difference in the members that are supported under the @<CPUUtilization>@ type.
    instanceHealthList :: Lude.Maybe [SingleInstanceHealth],
    -- | Pagination token for the next page of results, if available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Lude.Maybe Lude.DateTime,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancesHealthResponse' with the minimum fields required to make a request.
--
-- * 'instanceHealthList' - Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments. There is a difference in the members that are supported under the @<CPUUtilization>@ type.
-- * 'nextToken' - Pagination token for the next page of results, if available.
-- * 'refreshedAt' - The date and time that the health information was retrieved.
-- * 'responseStatus' - The response status code.
mkDescribeInstancesHealthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancesHealthResponse
mkDescribeInstancesHealthResponse pResponseStatus_ =
  DescribeInstancesHealthResponse'
    { instanceHealthList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      refreshedAt = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments. There is a difference in the members that are supported under the @<CPUUtilization>@ type.
--
-- /Note:/ Consider using 'instanceHealthList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsInstanceHealthList :: Lens.Lens' DescribeInstancesHealthResponse (Lude.Maybe [SingleInstanceHealth])
dihrsInstanceHealthList = Lens.lens (instanceHealthList :: DescribeInstancesHealthResponse -> Lude.Maybe [SingleInstanceHealth]) (\s a -> s {instanceHealthList = a} :: DescribeInstancesHealthResponse)
{-# DEPRECATED dihrsInstanceHealthList "Use generic-lens or generic-optics with 'instanceHealthList' instead." #-}

-- | Pagination token for the next page of results, if available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsNextToken :: Lens.Lens' DescribeInstancesHealthResponse (Lude.Maybe Lude.Text)
dihrsNextToken = Lens.lens (nextToken :: DescribeInstancesHealthResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancesHealthResponse)
{-# DEPRECATED dihrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The date and time that the health information was retrieved.
--
-- /Note:/ Consider using 'refreshedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsRefreshedAt :: Lens.Lens' DescribeInstancesHealthResponse (Lude.Maybe Lude.DateTime)
dihrsRefreshedAt = Lens.lens (refreshedAt :: DescribeInstancesHealthResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {refreshedAt = a} :: DescribeInstancesHealthResponse)
{-# DEPRECATED dihrsRefreshedAt "Use generic-lens or generic-optics with 'refreshedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrsResponseStatus :: Lens.Lens' DescribeInstancesHealthResponse Lude.Int
dihrsResponseStatus = Lens.lens (responseStatus :: DescribeInstancesHealthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancesHealthResponse)
{-# DEPRECATED dihrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
