{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement group in which to launch instances. The strategy of the placement group determines how the instances are organized within the group.
--
-- A @cluster@ placement group is a logical grouping of instances within a single Availability Zone that benefit from low network latency, high network throughput. A @spread@ placement group places instances on distinct hardware. A @partition@ placement group places groups of instances in different partitions, where instances in one partition do not share the same hardware with instances in another partition.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreatePlacementGroup
  ( -- * Creating a request
    CreatePlacementGroup (..),
    mkCreatePlacementGroup,

    -- ** Request lenses
    cpgStrategy,
    cpgTagSpecifications,
    cpgGroupName,
    cpgDryRun,
    cpgPartitionCount,

    -- * Destructuring the response
    CreatePlacementGroupResponse (..),
    mkCreatePlacementGroupResponse,

    -- ** Response lenses
    cpgrsPlacementGroup,
    cpgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { strategy ::
      Lude.Maybe PlacementStrategy,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    groupName :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    partitionCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlacementGroup' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupName' - A name for the placement group. Must be unique within the scope of your account for the Region.
--
-- Constraints: Up to 255 ASCII characters
-- * 'partitionCount' - The number of partitions. Valid only when __Strategy__ is set to @partition@ .
-- * 'strategy' - The placement strategy.
-- * 'tagSpecifications' - The tags to apply to the new placement group.
mkCreatePlacementGroup ::
  CreatePlacementGroup
mkCreatePlacementGroup =
  CreatePlacementGroup'
    { strategy = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      groupName = Lude.Nothing,
      dryRun = Lude.Nothing,
      partitionCount = Lude.Nothing
    }

-- | The placement strategy.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgStrategy :: Lens.Lens' CreatePlacementGroup (Lude.Maybe PlacementStrategy)
cpgStrategy = Lens.lens (strategy :: CreatePlacementGroup -> Lude.Maybe PlacementStrategy) (\s a -> s {strategy = a} :: CreatePlacementGroup)
{-# DEPRECATED cpgStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The tags to apply to the new placement group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgTagSpecifications :: Lens.Lens' CreatePlacementGroup (Lude.Maybe [TagSpecification])
cpgTagSpecifications = Lens.lens (tagSpecifications :: CreatePlacementGroup -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreatePlacementGroup)
{-# DEPRECATED cpgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A name for the placement group. Must be unique within the scope of your account for the Region.
--
-- Constraints: Up to 255 ASCII characters
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgGroupName :: Lens.Lens' CreatePlacementGroup (Lude.Maybe Lude.Text)
cpgGroupName = Lens.lens (groupName :: CreatePlacementGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: CreatePlacementGroup)
{-# DEPRECATED cpgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDryRun :: Lens.Lens' CreatePlacementGroup (Lude.Maybe Lude.Bool)
cpgDryRun = Lens.lens (dryRun :: CreatePlacementGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreatePlacementGroup)
{-# DEPRECATED cpgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The number of partitions. Valid only when __Strategy__ is set to @partition@ .
--
-- /Note:/ Consider using 'partitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgPartitionCount :: Lens.Lens' CreatePlacementGroup (Lude.Maybe Lude.Int)
cpgPartitionCount = Lens.lens (partitionCount :: CreatePlacementGroup -> Lude.Maybe Lude.Int) (\s a -> s {partitionCount = a} :: CreatePlacementGroup)
{-# DEPRECATED cpgPartitionCount "Use generic-lens or generic-optics with 'partitionCount' instead." #-}

instance Lude.AWSRequest CreatePlacementGroup where
  type Rs CreatePlacementGroup = CreatePlacementGroupResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreatePlacementGroupResponse'
            Lude.<$> (x Lude..@? "placementGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlacementGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePlacementGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlacementGroup where
  toQuery CreatePlacementGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePlacementGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Strategy" Lude.=: strategy,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "GroupName" Lude.=: groupName,
        "DryRun" Lude.=: dryRun,
        "PartitionCount" Lude.=: partitionCount
      ]

-- | /See:/ 'mkCreatePlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse = CreatePlacementGroupResponse'
  { placementGroup ::
      Lude.Maybe PlacementGroup,
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

-- | Creates a value of 'CreatePlacementGroupResponse' with the minimum fields required to make a request.
--
-- * 'placementGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreatePlacementGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlacementGroupResponse
mkCreatePlacementGroupResponse pResponseStatus_ =
  CreatePlacementGroupResponse'
    { placementGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrsPlacementGroup :: Lens.Lens' CreatePlacementGroupResponse (Lude.Maybe PlacementGroup)
cpgrsPlacementGroup = Lens.lens (placementGroup :: CreatePlacementGroupResponse -> Lude.Maybe PlacementGroup) (\s a -> s {placementGroup = a} :: CreatePlacementGroupResponse)
{-# DEPRECATED cpgrsPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrsResponseStatus :: Lens.Lens' CreatePlacementGroupResponse Lude.Int
cpgrsResponseStatus = Lens.lens (responseStatus :: CreatePlacementGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlacementGroupResponse)
{-# DEPRECATED cpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
