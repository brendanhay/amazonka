{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeEC2InstanceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the following information for the specified EC2 instance type:
--
--
--     * Maximum number of instances allowed per AWS account (service limit).
--
--
--     * Current usage for the AWS account.
--
--
-- To learn more about the capabilities of each instance type, see <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> . Note that the instance types offered may vary depending on the region.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.DescribeEC2InstanceLimits
  ( -- * Creating a request
    DescribeEC2InstanceLimits (..),
    mkDescribeEC2InstanceLimits,

    -- ** Request lenses
    deilEC2InstanceType,

    -- * Destructuring the response
    DescribeEC2InstanceLimitsResponse (..),
    mkDescribeEC2InstanceLimitsResponse,

    -- ** Response lenses
    deilrsEC2InstanceLimits,
    deilrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeEC2InstanceLimits' smart constructor.
newtype DescribeEC2InstanceLimits = DescribeEC2InstanceLimits'
  { -- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions. Leave this parameter blank to retrieve limits for all types.
    ec2InstanceType :: Lude.Maybe EC2InstanceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEC2InstanceLimits' with the minimum fields required to make a request.
--
-- * 'ec2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions. Leave this parameter blank to retrieve limits for all types.
mkDescribeEC2InstanceLimits ::
  DescribeEC2InstanceLimits
mkDescribeEC2InstanceLimits =
  DescribeEC2InstanceLimits' {ec2InstanceType = Lude.Nothing}

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions. Leave this parameter blank to retrieve limits for all types.
--
-- /Note:/ Consider using 'ec2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deilEC2InstanceType :: Lens.Lens' DescribeEC2InstanceLimits (Lude.Maybe EC2InstanceType)
deilEC2InstanceType = Lens.lens (ec2InstanceType :: DescribeEC2InstanceLimits -> Lude.Maybe EC2InstanceType) (\s a -> s {ec2InstanceType = a} :: DescribeEC2InstanceLimits)
{-# DEPRECATED deilEC2InstanceType "Use generic-lens or generic-optics with 'ec2InstanceType' instead." #-}

instance Lude.AWSRequest DescribeEC2InstanceLimits where
  type
    Rs DescribeEC2InstanceLimits =
      DescribeEC2InstanceLimitsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEC2InstanceLimitsResponse'
            Lude.<$> (x Lude..?> "EC2InstanceLimits" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEC2InstanceLimits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeEC2InstanceLimits" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEC2InstanceLimits where
  toJSON DescribeEC2InstanceLimits' {..} =
    Lude.object
      ( Lude.catMaybes
          [("EC2InstanceType" Lude..=) Lude.<$> ec2InstanceType]
      )

instance Lude.ToPath DescribeEC2InstanceLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEC2InstanceLimits where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeEC2InstanceLimitsResponse' smart constructor.
data DescribeEC2InstanceLimitsResponse = DescribeEC2InstanceLimitsResponse'
  { -- | The maximum number of instances for the specified instance type.
    ec2InstanceLimits :: Lude.Maybe [EC2InstanceLimit],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEC2InstanceLimitsResponse' with the minimum fields required to make a request.
--
-- * 'ec2InstanceLimits' - The maximum number of instances for the specified instance type.
-- * 'responseStatus' - The response status code.
mkDescribeEC2InstanceLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEC2InstanceLimitsResponse
mkDescribeEC2InstanceLimitsResponse pResponseStatus_ =
  DescribeEC2InstanceLimitsResponse'
    { ec2InstanceLimits =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The maximum number of instances for the specified instance type.
--
-- /Note:/ Consider using 'ec2InstanceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deilrsEC2InstanceLimits :: Lens.Lens' DescribeEC2InstanceLimitsResponse (Lude.Maybe [EC2InstanceLimit])
deilrsEC2InstanceLimits = Lens.lens (ec2InstanceLimits :: DescribeEC2InstanceLimitsResponse -> Lude.Maybe [EC2InstanceLimit]) (\s a -> s {ec2InstanceLimits = a} :: DescribeEC2InstanceLimitsResponse)
{-# DEPRECATED deilrsEC2InstanceLimits "Use generic-lens or generic-optics with 'ec2InstanceLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deilrsResponseStatus :: Lens.Lens' DescribeEC2InstanceLimitsResponse Lude.Int
deilrsResponseStatus = Lens.lens (responseStatus :: DescribeEC2InstanceLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEC2InstanceLimitsResponse)
{-# DEPRECATED deilrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
