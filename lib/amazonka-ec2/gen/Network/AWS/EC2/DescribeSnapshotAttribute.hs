{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified snapshot. You can specify only one attribute at a time.
--
-- For more information about EBS snapshots, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS Snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeSnapshotAttribute
  ( -- * Creating a request
    DescribeSnapshotAttribute (..),
    mkDescribeSnapshotAttribute,

    -- ** Request lenses
    dsaAttribute,
    dsaDryRun,
    dsaSnapshotId,

    -- * Destructuring the response
    DescribeSnapshotAttributeResponse (..),
    mkDescribeSnapshotAttributeResponse,

    -- ** Response lenses
    dsarsCreateVolumePermissions,
    dsarsProductCodes,
    dsarsSnapshotId,
    dsarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSnapshotAttribute' smart constructor.
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
  { -- | The snapshot attribute you would like to view.
    attribute :: SnapshotAttributeName,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The ID of the EBS snapshot.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The snapshot attribute you would like to view.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'snapshotId' - The ID of the EBS snapshot.
mkDescribeSnapshotAttribute ::
  -- | 'attribute'
  SnapshotAttributeName ->
  -- | 'snapshotId'
  Lude.Text ->
  DescribeSnapshotAttribute
mkDescribeSnapshotAttribute pAttribute_ pSnapshotId_ =
  DescribeSnapshotAttribute'
    { attribute = pAttribute_,
      dryRun = Lude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | The snapshot attribute you would like to view.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAttribute :: Lens.Lens' DescribeSnapshotAttribute SnapshotAttributeName
dsaAttribute = Lens.lens (attribute :: DescribeSnapshotAttribute -> SnapshotAttributeName) (\s a -> s {attribute = a} :: DescribeSnapshotAttribute)
{-# DEPRECATED dsaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaDryRun :: Lens.Lens' DescribeSnapshotAttribute (Lude.Maybe Lude.Bool)
dsaDryRun = Lens.lens (dryRun :: DescribeSnapshotAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSnapshotAttribute)
{-# DEPRECATED dsaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaSnapshotId :: Lens.Lens' DescribeSnapshotAttribute Lude.Text
dsaSnapshotId = Lens.lens (snapshotId :: DescribeSnapshotAttribute -> Lude.Text) (\s a -> s {snapshotId = a} :: DescribeSnapshotAttribute)
{-# DEPRECATED dsaSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest DescribeSnapshotAttribute where
  type
    Rs DescribeSnapshotAttribute =
      DescribeSnapshotAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSnapshotAttributeResponse'
            Lude.<$> ( x Lude..@? "createVolumePermission" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "snapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshotAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSnapshotAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshotAttribute where
  toQuery DescribeSnapshotAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSnapshotAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "DryRun" Lude.=: dryRun,
        "SnapshotId" Lude.=: snapshotId
      ]

-- | /See:/ 'mkDescribeSnapshotAttributeResponse' smart constructor.
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
  { -- | The users and groups that have the permissions for creating volumes from the snapshot.
    createVolumePermissions :: Lude.Maybe [CreateVolumePermission],
    -- | The product codes.
    productCodes :: Lude.Maybe [ProductCode],
    -- | The ID of the EBS snapshot.
    snapshotId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotAttributeResponse' with the minimum fields required to make a request.
--
-- * 'createVolumePermissions' - The users and groups that have the permissions for creating volumes from the snapshot.
-- * 'productCodes' - The product codes.
-- * 'snapshotId' - The ID of the EBS snapshot.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotAttributeResponse
mkDescribeSnapshotAttributeResponse pResponseStatus_ =
  DescribeSnapshotAttributeResponse'
    { createVolumePermissions =
        Lude.Nothing,
      productCodes = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The users and groups that have the permissions for creating volumes from the snapshot.
--
-- /Note:/ Consider using 'createVolumePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsCreateVolumePermissions :: Lens.Lens' DescribeSnapshotAttributeResponse (Lude.Maybe [CreateVolumePermission])
dsarsCreateVolumePermissions = Lens.lens (createVolumePermissions :: DescribeSnapshotAttributeResponse -> Lude.Maybe [CreateVolumePermission]) (\s a -> s {createVolumePermissions = a} :: DescribeSnapshotAttributeResponse)
{-# DEPRECATED dsarsCreateVolumePermissions "Use generic-lens or generic-optics with 'createVolumePermissions' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsProductCodes :: Lens.Lens' DescribeSnapshotAttributeResponse (Lude.Maybe [ProductCode])
dsarsProductCodes = Lens.lens (productCodes :: DescribeSnapshotAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeSnapshotAttributeResponse)
{-# DEPRECATED dsarsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The ID of the EBS snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsSnapshotId :: Lens.Lens' DescribeSnapshotAttributeResponse (Lude.Maybe Lude.Text)
dsarsSnapshotId = Lens.lens (snapshotId :: DescribeSnapshotAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: DescribeSnapshotAttributeResponse)
{-# DEPRECATED dsarsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DescribeSnapshotAttributeResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotAttributeResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
