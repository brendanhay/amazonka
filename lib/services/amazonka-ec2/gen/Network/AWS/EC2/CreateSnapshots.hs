{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates crash-consistent snapshots of multiple EBS volumes and stores
-- the data in S3. Volumes are chosen by specifying an instance. Any
-- attached volumes will produce one snapshot each that is crash-consistent
-- across the instance. Boot volumes can be excluded by changing the
-- parameters.
--
-- You can create multi-volume snapshots of instances in a Region and
-- instances on an Outpost. If you create snapshots from an instance in a
-- Region, the snapshots must be stored in the same Region as the instance.
-- If you create snapshots from an instance on an Outpost, the snapshots
-- can be stored on the same Outpost as the instance, or in the Region for
-- that Outpost.
module Network.AWS.EC2.CreateSnapshots
  ( -- * Creating a Request
    CreateSnapshots (..),
    newCreateSnapshots,

    -- * Request Lenses
    createSnapshots_outpostArn,
    createSnapshots_tagSpecifications,
    createSnapshots_copyTagsFromSource,
    createSnapshots_description,
    createSnapshots_dryRun,
    createSnapshots_instanceSpecification,

    -- * Destructuring the Response
    CreateSnapshotsResponse (..),
    newCreateSnapshotsResponse,

    -- * Response Lenses
    createSnapshotsResponse_snapshots,
    createSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSnapshots' smart constructor.
data CreateSnapshots = CreateSnapshots'
  { -- | The Amazon Resource Name (ARN) of the Outpost on which to create the
    -- local snapshots.
    --
    -- -   To create snapshots from an instance in a Region, omit this
    --     parameter. The snapshots are created in the same Region as the
    --     instance.
    --
    -- -   To create snapshots from an instance on an Outpost and store the
    --     snapshots in the Region, omit this parameter. The snapshots are
    --     created in the Region for the Outpost.
    --
    -- -   To create snapshots from an instance on an Outpost and store the
    --     snapshots on an Outpost, specify the ARN of the destination Outpost.
    --     The snapshots must be created on the same Outpost as the instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-multivol-snapshot Create multi-volume local snapshots from instances on an Outpost>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Tags to apply to every snapshot specified by the instance.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Copies the tags from the specified volume to corresponding snapshot.
    copyTagsFromSource :: Prelude.Maybe CopyTagsFromSource,
    -- | A description propagated to every snapshot specified by the instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance to specify which volumes should be included in the
    -- snapshots.
    instanceSpecification :: InstanceSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostArn', 'createSnapshots_outpostArn' - The Amazon Resource Name (ARN) of the Outpost on which to create the
-- local snapshots.
--
-- -   To create snapshots from an instance in a Region, omit this
--     parameter. The snapshots are created in the same Region as the
--     instance.
--
-- -   To create snapshots from an instance on an Outpost and store the
--     snapshots in the Region, omit this parameter. The snapshots are
--     created in the Region for the Outpost.
--
-- -   To create snapshots from an instance on an Outpost and store the
--     snapshots on an Outpost, specify the ARN of the destination Outpost.
--     The snapshots must be created on the same Outpost as the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-multivol-snapshot Create multi-volume local snapshots from instances on an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'tagSpecifications', 'createSnapshots_tagSpecifications' - Tags to apply to every snapshot specified by the instance.
--
-- 'copyTagsFromSource', 'createSnapshots_copyTagsFromSource' - Copies the tags from the specified volume to corresponding snapshot.
--
-- 'description', 'createSnapshots_description' - A description propagated to every snapshot specified by the instance.
--
-- 'dryRun', 'createSnapshots_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceSpecification', 'createSnapshots_instanceSpecification' - The instance to specify which volumes should be included in the
-- snapshots.
newCreateSnapshots ::
  -- | 'instanceSpecification'
  InstanceSpecification ->
  CreateSnapshots
newCreateSnapshots pInstanceSpecification_ =
  CreateSnapshots'
    { outpostArn = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      copyTagsFromSource = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceSpecification = pInstanceSpecification_
    }

-- | The Amazon Resource Name (ARN) of the Outpost on which to create the
-- local snapshots.
--
-- -   To create snapshots from an instance in a Region, omit this
--     parameter. The snapshots are created in the same Region as the
--     instance.
--
-- -   To create snapshots from an instance on an Outpost and store the
--     snapshots in the Region, omit this parameter. The snapshots are
--     created in the Region for the Outpost.
--
-- -   To create snapshots from an instance on an Outpost and store the
--     snapshots on an Outpost, specify the ARN of the destination Outpost.
--     The snapshots must be created on the same Outpost as the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-multivol-snapshot Create multi-volume local snapshots from instances on an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
createSnapshots_outpostArn :: Lens.Lens' CreateSnapshots (Prelude.Maybe Prelude.Text)
createSnapshots_outpostArn = Lens.lens (\CreateSnapshots' {outpostArn} -> outpostArn) (\s@CreateSnapshots' {} a -> s {outpostArn = a} :: CreateSnapshots)

-- | Tags to apply to every snapshot specified by the instance.
createSnapshots_tagSpecifications :: Lens.Lens' CreateSnapshots (Prelude.Maybe [TagSpecification])
createSnapshots_tagSpecifications = Lens.lens (\CreateSnapshots' {tagSpecifications} -> tagSpecifications) (\s@CreateSnapshots' {} a -> s {tagSpecifications = a} :: CreateSnapshots) Prelude.. Lens.mapping Lens.coerced

-- | Copies the tags from the specified volume to corresponding snapshot.
createSnapshots_copyTagsFromSource :: Lens.Lens' CreateSnapshots (Prelude.Maybe CopyTagsFromSource)
createSnapshots_copyTagsFromSource = Lens.lens (\CreateSnapshots' {copyTagsFromSource} -> copyTagsFromSource) (\s@CreateSnapshots' {} a -> s {copyTagsFromSource = a} :: CreateSnapshots)

-- | A description propagated to every snapshot specified by the instance.
createSnapshots_description :: Lens.Lens' CreateSnapshots (Prelude.Maybe Prelude.Text)
createSnapshots_description = Lens.lens (\CreateSnapshots' {description} -> description) (\s@CreateSnapshots' {} a -> s {description = a} :: CreateSnapshots)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSnapshots_dryRun :: Lens.Lens' CreateSnapshots (Prelude.Maybe Prelude.Bool)
createSnapshots_dryRun = Lens.lens (\CreateSnapshots' {dryRun} -> dryRun) (\s@CreateSnapshots' {} a -> s {dryRun = a} :: CreateSnapshots)

-- | The instance to specify which volumes should be included in the
-- snapshots.
createSnapshots_instanceSpecification :: Lens.Lens' CreateSnapshots InstanceSpecification
createSnapshots_instanceSpecification = Lens.lens (\CreateSnapshots' {instanceSpecification} -> instanceSpecification) (\s@CreateSnapshots' {} a -> s {instanceSpecification = a} :: CreateSnapshots)

instance Core.AWSRequest CreateSnapshots where
  type
    AWSResponse CreateSnapshots =
      CreateSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSnapshotsResponse'
            Prelude.<$> ( x Core..@? "snapshotSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshots

instance Prelude.NFData CreateSnapshots

instance Core.ToHeaders CreateSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSnapshots where
  toQuery CreateSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateSnapshots" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "OutpostArn" Core.=: outpostArn,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "CopyTagsFromSource" Core.=: copyTagsFromSource,
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        "InstanceSpecification"
          Core.=: instanceSpecification
      ]

-- | /See:/ 'newCreateSnapshotsResponse' smart constructor.
data CreateSnapshotsResponse = CreateSnapshotsResponse'
  { -- | List of snapshots.
    snapshots :: Prelude.Maybe [SnapshotInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshots', 'createSnapshotsResponse_snapshots' - List of snapshots.
--
-- 'httpStatus', 'createSnapshotsResponse_httpStatus' - The response's http status code.
newCreateSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotsResponse
newCreateSnapshotsResponse pHttpStatus_ =
  CreateSnapshotsResponse'
    { snapshots =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of snapshots.
createSnapshotsResponse_snapshots :: Lens.Lens' CreateSnapshotsResponse (Prelude.Maybe [SnapshotInfo])
createSnapshotsResponse_snapshots = Lens.lens (\CreateSnapshotsResponse' {snapshots} -> snapshots) (\s@CreateSnapshotsResponse' {} a -> s {snapshots = a} :: CreateSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSnapshotsResponse_httpStatus :: Lens.Lens' CreateSnapshotsResponse Prelude.Int
createSnapshotsResponse_httpStatus = Lens.lens (\CreateSnapshotsResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotsResponse' {} a -> s {httpStatus = a} :: CreateSnapshotsResponse)

instance Prelude.NFData CreateSnapshotsResponse
