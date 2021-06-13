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
-- Module      : Network.AWS.EC2.EnableFastSnapshotRestores
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables fast snapshot restores for the specified snapshots in the
-- specified Availability Zones.
--
-- You get the full benefit of fast snapshot restores after they enter the
-- @enabled@ state. To get the current state of fast snapshot restores, use
-- DescribeFastSnapshotRestores. To disable fast snapshot restores, use
-- DisableFastSnapshotRestores.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-fast-snapshot-restore.html Amazon EBS fast snapshot restore>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.EnableFastSnapshotRestores
  ( -- * Creating a Request
    EnableFastSnapshotRestores (..),
    newEnableFastSnapshotRestores,

    -- * Request Lenses
    enableFastSnapshotRestores_dryRun,
    enableFastSnapshotRestores_availabilityZones,
    enableFastSnapshotRestores_sourceSnapshotIds,

    -- * Destructuring the Response
    EnableFastSnapshotRestoresResponse (..),
    newEnableFastSnapshotRestoresResponse,

    -- * Response Lenses
    enableFastSnapshotRestoresResponse_unsuccessful,
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableFastSnapshotRestores' smart constructor.
data EnableFastSnapshotRestores = EnableFastSnapshotRestores'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more Availability Zones. For example, @us-east-2a@.
    availabilityZones :: [Prelude.Text],
    -- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
    -- You can specify a snapshot that was shared with you from another AWS
    -- account.
    sourceSnapshotIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableFastSnapshotRestores_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'availabilityZones', 'enableFastSnapshotRestores_availabilityZones' - One or more Availability Zones. For example, @us-east-2a@.
--
-- 'sourceSnapshotIds', 'enableFastSnapshotRestores_sourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
-- You can specify a snapshot that was shared with you from another AWS
-- account.
newEnableFastSnapshotRestores ::
  EnableFastSnapshotRestores
newEnableFastSnapshotRestores =
  EnableFastSnapshotRestores'
    { dryRun =
        Prelude.Nothing,
      availabilityZones = Prelude.mempty,
      sourceSnapshotIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableFastSnapshotRestores_dryRun :: Lens.Lens' EnableFastSnapshotRestores (Prelude.Maybe Prelude.Bool)
enableFastSnapshotRestores_dryRun = Lens.lens (\EnableFastSnapshotRestores' {dryRun} -> dryRun) (\s@EnableFastSnapshotRestores' {} a -> s {dryRun = a} :: EnableFastSnapshotRestores)

-- | One or more Availability Zones. For example, @us-east-2a@.
enableFastSnapshotRestores_availabilityZones :: Lens.Lens' EnableFastSnapshotRestores [Prelude.Text]
enableFastSnapshotRestores_availabilityZones = Lens.lens (\EnableFastSnapshotRestores' {availabilityZones} -> availabilityZones) (\s@EnableFastSnapshotRestores' {} a -> s {availabilityZones = a} :: EnableFastSnapshotRestores) Prelude.. Lens._Coerce

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
-- You can specify a snapshot that was shared with you from another AWS
-- account.
enableFastSnapshotRestores_sourceSnapshotIds :: Lens.Lens' EnableFastSnapshotRestores [Prelude.Text]
enableFastSnapshotRestores_sourceSnapshotIds = Lens.lens (\EnableFastSnapshotRestores' {sourceSnapshotIds} -> sourceSnapshotIds) (\s@EnableFastSnapshotRestores' {} a -> s {sourceSnapshotIds = a} :: EnableFastSnapshotRestores) Prelude.. Lens._Coerce

instance Core.AWSRequest EnableFastSnapshotRestores where
  type
    AWSResponse EnableFastSnapshotRestores =
      EnableFastSnapshotRestoresResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          EnableFastSnapshotRestoresResponse'
            Prelude.<$> ( x Core..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "successful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableFastSnapshotRestores

instance Prelude.NFData EnableFastSnapshotRestores

instance Core.ToHeaders EnableFastSnapshotRestores where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath EnableFastSnapshotRestores where
  toPath = Prelude.const "/"

instance Core.ToQuery EnableFastSnapshotRestores where
  toQuery EnableFastSnapshotRestores' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("EnableFastSnapshotRestores" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList
          "AvailabilityZone"
          availabilityZones,
        Core.toQueryList
          "SourceSnapshotId"
          sourceSnapshotIds
      ]

-- | /See:/ 'newEnableFastSnapshotRestoresResponse' smart constructor.
data EnableFastSnapshotRestoresResponse = EnableFastSnapshotRestoresResponse'
  { -- | Information about the snapshots for which fast snapshot restores could
    -- not be enabled.
    unsuccessful :: Prelude.Maybe [EnableFastSnapshotRestoreErrorItem],
    -- | Information about the snapshots for which fast snapshot restores were
    -- successfully enabled.
    successful :: Prelude.Maybe [EnableFastSnapshotRestoreSuccessItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'enableFastSnapshotRestoresResponse_unsuccessful' - Information about the snapshots for which fast snapshot restores could
-- not be enabled.
--
-- 'successful', 'enableFastSnapshotRestoresResponse_successful' - Information about the snapshots for which fast snapshot restores were
-- successfully enabled.
--
-- 'httpStatus', 'enableFastSnapshotRestoresResponse_httpStatus' - The response's http status code.
newEnableFastSnapshotRestoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableFastSnapshotRestoresResponse
newEnableFastSnapshotRestoresResponse pHttpStatus_ =
  EnableFastSnapshotRestoresResponse'
    { unsuccessful =
        Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could
-- not be enabled.
enableFastSnapshotRestoresResponse_unsuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Prelude.Maybe [EnableFastSnapshotRestoreErrorItem])
enableFastSnapshotRestoresResponse_unsuccessful = Lens.lens (\EnableFastSnapshotRestoresResponse' {unsuccessful} -> unsuccessful) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {unsuccessful = a} :: EnableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the snapshots for which fast snapshot restores were
-- successfully enabled.
enableFastSnapshotRestoresResponse_successful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Prelude.Maybe [EnableFastSnapshotRestoreSuccessItem])
enableFastSnapshotRestoresResponse_successful = Lens.lens (\EnableFastSnapshotRestoresResponse' {successful} -> successful) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {successful = a} :: EnableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
enableFastSnapshotRestoresResponse_httpStatus :: Lens.Lens' EnableFastSnapshotRestoresResponse Prelude.Int
enableFastSnapshotRestoresResponse_httpStatus = Lens.lens (\EnableFastSnapshotRestoresResponse' {httpStatus} -> httpStatus) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {httpStatus = a} :: EnableFastSnapshotRestoresResponse)

instance
  Prelude.NFData
    EnableFastSnapshotRestoresResponse
