{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates crash-consistent snapshots of multiple EBS volumes and stores the data in S3. Volumes are chosen by specifying an instance. Any attached volumes will produce one snapshot each that is crash-consistent across the instance. Boot volumes can be excluded by changing the parameters.
module Network.AWS.EC2.CreateSnapshots
  ( -- * Creating a request
    CreateSnapshots (..),
    mkCreateSnapshots,

    -- ** Request lenses
    csTagSpecifications,
    csInstanceSpecification,
    csCopyTagsFromSource,
    csDescription,
    csDryRun,

    -- * Destructuring the response
    CreateSnapshotsResponse (..),
    mkCreateSnapshotsResponse,

    -- ** Response lenses
    csrsSnapshots,
    csrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSnapshots' smart constructor.
data CreateSnapshots = CreateSnapshots'
  { -- | Tags to apply to every snapshot specified by the instance.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The instance to specify which volumes should be included in the snapshots.
    instanceSpecification :: InstanceSpecification,
    -- | Copies the tags from the specified volume to corresponding snapshot.
    copyTagsFromSource :: Lude.Maybe CopyTagsFromSource,
    -- | A description propagated to every snapshot specified by the instance.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshots' with the minimum fields required to make a request.
--
-- * 'tagSpecifications' - Tags to apply to every snapshot specified by the instance.
-- * 'instanceSpecification' - The instance to specify which volumes should be included in the snapshots.
-- * 'copyTagsFromSource' - Copies the tags from the specified volume to corresponding snapshot.
-- * 'description' - A description propagated to every snapshot specified by the instance.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateSnapshots ::
  -- | 'instanceSpecification'
  InstanceSpecification ->
  CreateSnapshots
mkCreateSnapshots pInstanceSpecification_ =
  CreateSnapshots'
    { tagSpecifications = Lude.Nothing,
      instanceSpecification = pInstanceSpecification_,
      copyTagsFromSource = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Tags to apply to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTagSpecifications :: Lens.Lens' CreateSnapshots (Lude.Maybe [TagSpecification])
csTagSpecifications = Lens.lens (tagSpecifications :: CreateSnapshots -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSnapshots)
{-# DEPRECATED csTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The instance to specify which volumes should be included in the snapshots.
--
-- /Note:/ Consider using 'instanceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceSpecification :: Lens.Lens' CreateSnapshots InstanceSpecification
csInstanceSpecification = Lens.lens (instanceSpecification :: CreateSnapshots -> InstanceSpecification) (\s a -> s {instanceSpecification = a} :: CreateSnapshots)
{-# DEPRECATED csInstanceSpecification "Use generic-lens or generic-optics with 'instanceSpecification' instead." #-}

-- | Copies the tags from the specified volume to corresponding snapshot.
--
-- /Note:/ Consider using 'copyTagsFromSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCopyTagsFromSource :: Lens.Lens' CreateSnapshots (Lude.Maybe CopyTagsFromSource)
csCopyTagsFromSource = Lens.lens (copyTagsFromSource :: CreateSnapshots -> Lude.Maybe CopyTagsFromSource) (\s a -> s {copyTagsFromSource = a} :: CreateSnapshots)
{-# DEPRECATED csCopyTagsFromSource "Use generic-lens or generic-optics with 'copyTagsFromSource' instead." #-}

-- | A description propagated to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSnapshots (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSnapshots)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDryRun :: Lens.Lens' CreateSnapshots (Lude.Maybe Lude.Bool)
csDryRun = Lens.lens (dryRun :: CreateSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSnapshots)
{-# DEPRECATED csDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateSnapshots where
  type Rs CreateSnapshots = CreateSnapshotsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateSnapshotsResponse'
            Lude.<$> ( x Lude..@? "snapshotSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshots where
  toQuery CreateSnapshots' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "InstanceSpecification" Lude.=: instanceSpecification,
        "CopyTagsFromSource" Lude.=: copyTagsFromSource,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateSnapshotsResponse' smart constructor.
data CreateSnapshotsResponse = CreateSnapshotsResponse'
  { -- | List of snapshots.
    snapshots :: Lude.Maybe [SnapshotInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'snapshots' - List of snapshots.
-- * 'responseStatus' - The response status code.
mkCreateSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotsResponse
mkCreateSnapshotsResponse pResponseStatus_ =
  CreateSnapshotsResponse'
    { snapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSnapshots :: Lens.Lens' CreateSnapshotsResponse (Lude.Maybe [SnapshotInfo])
csrsSnapshots = Lens.lens (snapshots :: CreateSnapshotsResponse -> Lude.Maybe [SnapshotInfo]) (\s a -> s {snapshots = a} :: CreateSnapshotsResponse)
{-# DEPRECATED csrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSnapshotsResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotsResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
