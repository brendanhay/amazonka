{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRDSDBInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon RDS instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
module Network.AWS.OpsWorks.DescribeRDSDBInstances
  ( -- * Creating a request
    DescribeRDSDBInstances (..),
    mkDescribeRDSDBInstances,

    -- ** Request lenses
    drdiRDSDBInstanceARNs,
    drdiStackId,

    -- * Destructuring the response
    DescribeRDSDBInstancesResponse (..),
    mkDescribeRDSDBInstancesResponse,

    -- ** Response lenses
    drdirsRDSDBInstances,
    drdirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRDSDBInstances' smart constructor.
data DescribeRDSDBInstances = DescribeRDSDBInstances'
  { rdsDBInstanceARNs ::
      Lude.Maybe [Lude.Text],
    stackId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRDSDBInstances' with the minimum fields required to make a request.
--
-- * 'rdsDBInstanceARNs' - An array containing the ARNs of the instances to be described.
-- * 'stackId' - The ID of the stack with which the instances are registered. The operation returns descriptions of all registered Amazon RDS instances.
mkDescribeRDSDBInstances ::
  -- | 'stackId'
  Lude.Text ->
  DescribeRDSDBInstances
mkDescribeRDSDBInstances pStackId_ =
  DescribeRDSDBInstances'
    { rdsDBInstanceARNs = Lude.Nothing,
      stackId = pStackId_
    }

-- | An array containing the ARNs of the instances to be described.
--
-- /Note:/ Consider using 'rdsDBInstanceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiRDSDBInstanceARNs :: Lens.Lens' DescribeRDSDBInstances (Lude.Maybe [Lude.Text])
drdiRDSDBInstanceARNs = Lens.lens (rdsDBInstanceARNs :: DescribeRDSDBInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {rdsDBInstanceARNs = a} :: DescribeRDSDBInstances)
{-# DEPRECATED drdiRDSDBInstanceARNs "Use generic-lens or generic-optics with 'rdsDBInstanceARNs' instead." #-}

-- | The ID of the stack with which the instances are registered. The operation returns descriptions of all registered Amazon RDS instances.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiStackId :: Lens.Lens' DescribeRDSDBInstances Lude.Text
drdiStackId = Lens.lens (stackId :: DescribeRDSDBInstances -> Lude.Text) (\s a -> s {stackId = a} :: DescribeRDSDBInstances)
{-# DEPRECATED drdiStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeRDSDBInstances where
  type Rs DescribeRDSDBInstances = DescribeRDSDBInstancesResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRDSDBInstancesResponse'
            Lude.<$> (x Lude..?> "RdsDbInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRDSDBInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeRdsDbInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRDSDBInstances where
  toJSON DescribeRDSDBInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RdsDbInstanceArns" Lude..=) Lude.<$> rdsDBInstanceARNs,
            Lude.Just ("StackId" Lude..= stackId)
          ]
      )

instance Lude.ToPath DescribeRDSDBInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRDSDBInstances where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeRdsDbInstances@ request.
--
-- /See:/ 'mkDescribeRDSDBInstancesResponse' smart constructor.
data DescribeRDSDBInstancesResponse = DescribeRDSDBInstancesResponse'
  { rdsDBInstances ::
      Lude.Maybe [RDSDBInstance],
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

-- | Creates a value of 'DescribeRDSDBInstancesResponse' with the minimum fields required to make a request.
--
-- * 'rdsDBInstances' - An a array of @RdsDbInstance@ objects that describe the instances.
-- * 'responseStatus' - The response status code.
mkDescribeRDSDBInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRDSDBInstancesResponse
mkDescribeRDSDBInstancesResponse pResponseStatus_ =
  DescribeRDSDBInstancesResponse'
    { rdsDBInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An a array of @RdsDbInstance@ objects that describe the instances.
--
-- /Note:/ Consider using 'rdsDBInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirsRDSDBInstances :: Lens.Lens' DescribeRDSDBInstancesResponse (Lude.Maybe [RDSDBInstance])
drdirsRDSDBInstances = Lens.lens (rdsDBInstances :: DescribeRDSDBInstancesResponse -> Lude.Maybe [RDSDBInstance]) (\s a -> s {rdsDBInstances = a} :: DescribeRDSDBInstancesResponse)
{-# DEPRECATED drdirsRDSDBInstances "Use generic-lens or generic-optics with 'rdsDBInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirsResponseStatus :: Lens.Lens' DescribeRDSDBInstancesResponse Lude.Int
drdirsResponseStatus = Lens.lens (responseStatus :: DescribeRDSDBInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRDSDBInstancesResponse)
{-# DEPRECATED drdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
