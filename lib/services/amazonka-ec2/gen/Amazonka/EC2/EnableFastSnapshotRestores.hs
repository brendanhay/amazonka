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
-- Module      : Amazonka.EC2.EnableFastSnapshotRestores
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.EnableFastSnapshotRestores
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
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_unsuccessful,
    enableFastSnapshotRestoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- You can specify a snapshot that was shared with you from another Amazon
    -- Web Services account.
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
-- You can specify a snapshot that was shared with you from another Amazon
-- Web Services account.
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
enableFastSnapshotRestores_availabilityZones = Lens.lens (\EnableFastSnapshotRestores' {availabilityZones} -> availabilityZones) (\s@EnableFastSnapshotRestores' {} a -> s {availabilityZones = a} :: EnableFastSnapshotRestores) Prelude.. Lens.coerced

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
-- You can specify a snapshot that was shared with you from another Amazon
-- Web Services account.
enableFastSnapshotRestores_sourceSnapshotIds :: Lens.Lens' EnableFastSnapshotRestores [Prelude.Text]
enableFastSnapshotRestores_sourceSnapshotIds = Lens.lens (\EnableFastSnapshotRestores' {sourceSnapshotIds} -> sourceSnapshotIds) (\s@EnableFastSnapshotRestores' {} a -> s {sourceSnapshotIds = a} :: EnableFastSnapshotRestores) Prelude.. Lens.coerced

instance Core.AWSRequest EnableFastSnapshotRestores where
  type
    AWSResponse EnableFastSnapshotRestores =
      EnableFastSnapshotRestoresResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableFastSnapshotRestoresResponse'
            Prelude.<$> ( x Data..@? "successful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x Data..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableFastSnapshotRestores where
  hashWithSalt _salt EnableFastSnapshotRestores' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` sourceSnapshotIds

instance Prelude.NFData EnableFastSnapshotRestores where
  rnf EnableFastSnapshotRestores' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf sourceSnapshotIds

instance Data.ToHeaders EnableFastSnapshotRestores where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableFastSnapshotRestores where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableFastSnapshotRestores where
  toQuery EnableFastSnapshotRestores' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableFastSnapshotRestores" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList
          "AvailabilityZone"
          availabilityZones,
        Data.toQueryList
          "SourceSnapshotId"
          sourceSnapshotIds
      ]

-- | /See:/ 'newEnableFastSnapshotRestoresResponse' smart constructor.
data EnableFastSnapshotRestoresResponse = EnableFastSnapshotRestoresResponse'
  { -- | Information about the snapshots for which fast snapshot restores were
    -- successfully enabled.
    successful :: Prelude.Maybe [EnableFastSnapshotRestoreSuccessItem],
    -- | Information about the snapshots for which fast snapshot restores could
    -- not be enabled.
    unsuccessful :: Prelude.Maybe [EnableFastSnapshotRestoreErrorItem],
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
-- 'successful', 'enableFastSnapshotRestoresResponse_successful' - Information about the snapshots for which fast snapshot restores were
-- successfully enabled.
--
-- 'unsuccessful', 'enableFastSnapshotRestoresResponse_unsuccessful' - Information about the snapshots for which fast snapshot restores could
-- not be enabled.
--
-- 'httpStatus', 'enableFastSnapshotRestoresResponse_httpStatus' - The response's http status code.
newEnableFastSnapshotRestoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableFastSnapshotRestoresResponse
newEnableFastSnapshotRestoresResponse pHttpStatus_ =
  EnableFastSnapshotRestoresResponse'
    { successful =
        Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the snapshots for which fast snapshot restores were
-- successfully enabled.
enableFastSnapshotRestoresResponse_successful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Prelude.Maybe [EnableFastSnapshotRestoreSuccessItem])
enableFastSnapshotRestoresResponse_successful = Lens.lens (\EnableFastSnapshotRestoresResponse' {successful} -> successful) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {successful = a} :: EnableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the snapshots for which fast snapshot restores could
-- not be enabled.
enableFastSnapshotRestoresResponse_unsuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Prelude.Maybe [EnableFastSnapshotRestoreErrorItem])
enableFastSnapshotRestoresResponse_unsuccessful = Lens.lens (\EnableFastSnapshotRestoresResponse' {unsuccessful} -> unsuccessful) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {unsuccessful = a} :: EnableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
enableFastSnapshotRestoresResponse_httpStatus :: Lens.Lens' EnableFastSnapshotRestoresResponse Prelude.Int
enableFastSnapshotRestoresResponse_httpStatus = Lens.lens (\EnableFastSnapshotRestoresResponse' {httpStatus} -> httpStatus) (\s@EnableFastSnapshotRestoresResponse' {} a -> s {httpStatus = a} :: EnableFastSnapshotRestoresResponse)

instance
  Prelude.NFData
    EnableFastSnapshotRestoresResponse
  where
  rnf EnableFastSnapshotRestoresResponse' {..} =
    Prelude.rnf successful
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
