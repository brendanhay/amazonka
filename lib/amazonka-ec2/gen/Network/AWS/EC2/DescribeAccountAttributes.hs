{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes attributes of your AWS account. The following are the supported account attributes:
--
--
--     * @supported-platforms@ : Indicates whether your account can launch instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.
--
--
--     * @default-vpc@ : The ID of the default VPC for your account, or @none@ .
--
--
--     * @max-instances@ : This attribute is no longer supported. The returned value does not reflect your actual vCPU limit for running On-Demand Instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-on-demand-instances.html#ec2-on-demand-instances-limits On-Demand Instance Limits> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--     * @vpc-max-security-groups-per-interface@ : The maximum number of security groups that you can assign to a network interface.
--
--
--     * @max-elastic-ips@ : The maximum number of Elastic IP addresses that you can allocate for use with EC2-Classic.
--
--
--     * @vpc-max-elastic-ips@ : The maximum number of Elastic IP addresses that you can allocate for use with EC2-VPC.
module Network.AWS.EC2.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- ** Request lenses
    daaAttributeNames,
    daaDryRun,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsAccountAttributes,
    daarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  { attributeNames ::
      Lude.Maybe [AccountAttributeName],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - The account attribute names.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes =
  DescribeAccountAttributes'
    { attributeNames = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The account attribute names.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAttributeNames :: Lens.Lens' DescribeAccountAttributes (Lude.Maybe [AccountAttributeName])
daaAttributeNames = Lens.lens (attributeNames :: DescribeAccountAttributes -> Lude.Maybe [AccountAttributeName]) (\s a -> s {attributeNames = a} :: DescribeAccountAttributes)
{-# DEPRECATED daaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaDryRun :: Lens.Lens' DescribeAccountAttributes (Lude.Maybe Lude.Bool)
daaDryRun = Lens.lens (dryRun :: DescribeAccountAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeAccountAttributes)
{-# DEPRECATED daaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> ( x Lude..@? "accountAttributeSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery DescribeAccountAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAccountAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "AttributeName" Lude.<$> attributeNames),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountAttributes ::
      Lude.Maybe
        [AccountAttribute],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- * 'accountAttributes' - Information about the account attributes.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { accountAttributes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the account attributes.
--
-- /Note:/ Consider using 'accountAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAccountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe [AccountAttribute])
daarsAccountAttributes = Lens.lens (accountAttributes :: DescribeAccountAttributesResponse -> Lude.Maybe [AccountAttribute]) (\s a -> s {accountAttributes = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsAccountAttributes "Use generic-lens or generic-optics with 'accountAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
