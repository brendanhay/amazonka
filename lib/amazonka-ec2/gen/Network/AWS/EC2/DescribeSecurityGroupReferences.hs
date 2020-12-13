{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroupReferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the VPCs on the other side of a VPC peering connection that are referencing the security groups you've specified in this request.
module Network.AWS.EC2.DescribeSecurityGroupReferences
  ( -- * Creating a request
    DescribeSecurityGroupReferences (..),
    mkDescribeSecurityGroupReferences,

    -- ** Request lenses
    dsgrGroupId,
    dsgrDryRun,

    -- * Destructuring the response
    DescribeSecurityGroupReferencesResponse (..),
    mkDescribeSecurityGroupReferencesResponse,

    -- ** Response lenses
    dsgrrsSecurityGroupReferenceSet,
    dsgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSecurityGroupReferences' smart constructor.
data DescribeSecurityGroupReferences = DescribeSecurityGroupReferences'
  { -- | The IDs of the security groups in your account.
    groupId :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityGroupReferences' with the minimum fields required to make a request.
--
-- * 'groupId' - The IDs of the security groups in your account.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeSecurityGroupReferences ::
  DescribeSecurityGroupReferences
mkDescribeSecurityGroupReferences =
  DescribeSecurityGroupReferences'
    { groupId = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | The IDs of the security groups in your account.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrGroupId :: Lens.Lens' DescribeSecurityGroupReferences [Lude.Text]
dsgrGroupId = Lens.lens (groupId :: DescribeSecurityGroupReferences -> [Lude.Text]) (\s a -> s {groupId = a} :: DescribeSecurityGroupReferences)
{-# DEPRECATED dsgrGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrDryRun :: Lens.Lens' DescribeSecurityGroupReferences (Lude.Maybe Lude.Bool)
dsgrDryRun = Lens.lens (dryRun :: DescribeSecurityGroupReferences -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSecurityGroupReferences)
{-# DEPRECATED dsgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeSecurityGroupReferences where
  type
    Rs DescribeSecurityGroupReferences =
      DescribeSecurityGroupReferencesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSecurityGroupReferencesResponse'
            Lude.<$> ( x Lude..@? "securityGroupReferenceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSecurityGroupReferences where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSecurityGroupReferences where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSecurityGroupReferences where
  toQuery DescribeSecurityGroupReferences' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeSecurityGroupReferences" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "GroupId" groupId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeSecurityGroupReferencesResponse' smart constructor.
data DescribeSecurityGroupReferencesResponse = DescribeSecurityGroupReferencesResponse'
  { -- | Information about the VPCs with the referencing security groups.
    securityGroupReferenceSet :: Lude.Maybe [SecurityGroupReference],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityGroupReferencesResponse' with the minimum fields required to make a request.
--
-- * 'securityGroupReferenceSet' - Information about the VPCs with the referencing security groups.
-- * 'responseStatus' - The response status code.
mkDescribeSecurityGroupReferencesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSecurityGroupReferencesResponse
mkDescribeSecurityGroupReferencesResponse pResponseStatus_ =
  DescribeSecurityGroupReferencesResponse'
    { securityGroupReferenceSet =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPCs with the referencing security groups.
--
-- /Note:/ Consider using 'securityGroupReferenceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsSecurityGroupReferenceSet :: Lens.Lens' DescribeSecurityGroupReferencesResponse (Lude.Maybe [SecurityGroupReference])
dsgrrsSecurityGroupReferenceSet = Lens.lens (securityGroupReferenceSet :: DescribeSecurityGroupReferencesResponse -> Lude.Maybe [SecurityGroupReference]) (\s a -> s {securityGroupReferenceSet = a} :: DescribeSecurityGroupReferencesResponse)
{-# DEPRECATED dsgrrsSecurityGroupReferenceSet "Use generic-lens or generic-optics with 'securityGroupReferenceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DescribeSecurityGroupReferencesResponse Lude.Int
dsgrrsResponseStatus = Lens.lens (responseStatus :: DescribeSecurityGroupReferencesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecurityGroupReferencesResponse)
{-# DEPRECATED dsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
