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
-- Module      : Amazonka.EC2.DisableFastSnapshotRestores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables fast snapshot restores for the specified snapshots in the
-- specified Availability Zones.
module Amazonka.EC2.DisableFastSnapshotRestores
  ( -- * Creating a Request
    DisableFastSnapshotRestores (..),
    newDisableFastSnapshotRestores,

    -- * Request Lenses
    disableFastSnapshotRestores_dryRun,
    disableFastSnapshotRestores_availabilityZones,
    disableFastSnapshotRestores_sourceSnapshotIds,

    -- * Destructuring the Response
    DisableFastSnapshotRestoresResponse (..),
    newDisableFastSnapshotRestoresResponse,

    -- * Response Lenses
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_unsuccessful,
    disableFastSnapshotRestoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableFastSnapshotRestores' smart constructor.
data DisableFastSnapshotRestores = DisableFastSnapshotRestores'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more Availability Zones. For example, @us-east-2a@.
    availabilityZones :: [Prelude.Text],
    -- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
    sourceSnapshotIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableFastSnapshotRestores_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'availabilityZones', 'disableFastSnapshotRestores_availabilityZones' - One or more Availability Zones. For example, @us-east-2a@.
--
-- 'sourceSnapshotIds', 'disableFastSnapshotRestores_sourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
newDisableFastSnapshotRestores ::
  DisableFastSnapshotRestores
newDisableFastSnapshotRestores =
  DisableFastSnapshotRestores'
    { dryRun =
        Prelude.Nothing,
      availabilityZones = Prelude.mempty,
      sourceSnapshotIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableFastSnapshotRestores_dryRun :: Lens.Lens' DisableFastSnapshotRestores (Prelude.Maybe Prelude.Bool)
disableFastSnapshotRestores_dryRun = Lens.lens (\DisableFastSnapshotRestores' {dryRun} -> dryRun) (\s@DisableFastSnapshotRestores' {} a -> s {dryRun = a} :: DisableFastSnapshotRestores)

-- | One or more Availability Zones. For example, @us-east-2a@.
disableFastSnapshotRestores_availabilityZones :: Lens.Lens' DisableFastSnapshotRestores [Prelude.Text]
disableFastSnapshotRestores_availabilityZones = Lens.lens (\DisableFastSnapshotRestores' {availabilityZones} -> availabilityZones) (\s@DisableFastSnapshotRestores' {} a -> s {availabilityZones = a} :: DisableFastSnapshotRestores) Prelude.. Lens.coerced

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
disableFastSnapshotRestores_sourceSnapshotIds :: Lens.Lens' DisableFastSnapshotRestores [Prelude.Text]
disableFastSnapshotRestores_sourceSnapshotIds = Lens.lens (\DisableFastSnapshotRestores' {sourceSnapshotIds} -> sourceSnapshotIds) (\s@DisableFastSnapshotRestores' {} a -> s {sourceSnapshotIds = a} :: DisableFastSnapshotRestores) Prelude.. Lens.coerced

instance Core.AWSRequest DisableFastSnapshotRestores where
  type
    AWSResponse DisableFastSnapshotRestores =
      DisableFastSnapshotRestoresResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableFastSnapshotRestoresResponse'
            Prelude.<$> ( x Data..@? "successful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x Data..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableFastSnapshotRestores where
  hashWithSalt _salt DisableFastSnapshotRestores' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` sourceSnapshotIds

instance Prelude.NFData DisableFastSnapshotRestores where
  rnf DisableFastSnapshotRestores' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf availabilityZones `Prelude.seq`
        Prelude.rnf sourceSnapshotIds

instance Data.ToHeaders DisableFastSnapshotRestores where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableFastSnapshotRestores where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableFastSnapshotRestores where
  toQuery DisableFastSnapshotRestores' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisableFastSnapshotRestores" ::
                      Prelude.ByteString
                  ),
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

-- | /See:/ 'newDisableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { -- | Information about the snapshots for which fast snapshot restores were
    -- successfully disabled.
    successful :: Prelude.Maybe [DisableFastSnapshotRestoreSuccessItem],
    -- | Information about the snapshots for which fast snapshot restores could
    -- not be disabled.
    unsuccessful :: Prelude.Maybe [DisableFastSnapshotRestoreErrorItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'disableFastSnapshotRestoresResponse_successful' - Information about the snapshots for which fast snapshot restores were
-- successfully disabled.
--
-- 'unsuccessful', 'disableFastSnapshotRestoresResponse_unsuccessful' - Information about the snapshots for which fast snapshot restores could
-- not be disabled.
--
-- 'httpStatus', 'disableFastSnapshotRestoresResponse_httpStatus' - The response's http status code.
newDisableFastSnapshotRestoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableFastSnapshotRestoresResponse
newDisableFastSnapshotRestoresResponse pHttpStatus_ =
  DisableFastSnapshotRestoresResponse'
    { successful =
        Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the snapshots for which fast snapshot restores were
-- successfully disabled.
disableFastSnapshotRestoresResponse_successful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Prelude.Maybe [DisableFastSnapshotRestoreSuccessItem])
disableFastSnapshotRestoresResponse_successful = Lens.lens (\DisableFastSnapshotRestoresResponse' {successful} -> successful) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {successful = a} :: DisableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the snapshots for which fast snapshot restores could
-- not be disabled.
disableFastSnapshotRestoresResponse_unsuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Prelude.Maybe [DisableFastSnapshotRestoreErrorItem])
disableFastSnapshotRestoresResponse_unsuccessful = Lens.lens (\DisableFastSnapshotRestoresResponse' {unsuccessful} -> unsuccessful) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {unsuccessful = a} :: DisableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disableFastSnapshotRestoresResponse_httpStatus :: Lens.Lens' DisableFastSnapshotRestoresResponse Prelude.Int
disableFastSnapshotRestoresResponse_httpStatus = Lens.lens (\DisableFastSnapshotRestoresResponse' {httpStatus} -> httpStatus) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {httpStatus = a} :: DisableFastSnapshotRestoresResponse)

instance
  Prelude.NFData
    DisableFastSnapshotRestoresResponse
  where
  rnf DisableFastSnapshotRestoresResponse' {..} =
    Prelude.rnf successful `Prelude.seq`
      Prelude.rnf unsuccessful `Prelude.seq`
        Prelude.rnf httpStatus
