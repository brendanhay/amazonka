{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DisableFastSnapshotRestores
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables fast snapshot restores for the specified snapshots in the
-- specified Availability Zones.
module Network.AWS.EC2.DisableFastSnapshotRestores
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
    disableFastSnapshotRestoresResponse_unsuccessful,
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
disableFastSnapshotRestores_availabilityZones = Lens.lens (\DisableFastSnapshotRestores' {availabilityZones} -> availabilityZones) (\s@DisableFastSnapshotRestores' {} a -> s {availabilityZones = a} :: DisableFastSnapshotRestores) Prelude.. Prelude._Coerce

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@.
disableFastSnapshotRestores_sourceSnapshotIds :: Lens.Lens' DisableFastSnapshotRestores [Prelude.Text]
disableFastSnapshotRestores_sourceSnapshotIds = Lens.lens (\DisableFastSnapshotRestores' {sourceSnapshotIds} -> sourceSnapshotIds) (\s@DisableFastSnapshotRestores' {} a -> s {sourceSnapshotIds = a} :: DisableFastSnapshotRestores) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    DisableFastSnapshotRestores
  where
  type
    Rs DisableFastSnapshotRestores =
      DisableFastSnapshotRestoresResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisableFastSnapshotRestoresResponse'
            Prelude.<$> ( x Prelude..@? "unsuccessful"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> ( x Prelude..@? "successful"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableFastSnapshotRestores

instance Prelude.NFData DisableFastSnapshotRestores

instance
  Prelude.ToHeaders
    DisableFastSnapshotRestores
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisableFastSnapshotRestores where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableFastSnapshotRestores where
  toQuery DisableFastSnapshotRestores' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DisableFastSnapshotRestores" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQueryList
          "AvailabilityZone"
          availabilityZones,
        Prelude.toQueryList
          "SourceSnapshotId"
          sourceSnapshotIds
      ]

-- | /See:/ 'newDisableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { -- | Information about the snapshots for which fast snapshot restores could
    -- not be disabled.
    unsuccessful :: Prelude.Maybe [DisableFastSnapshotRestoreErrorItem],
    -- | Information about the snapshots for which fast snapshot restores were
    -- successfully disabled.
    successful :: Prelude.Maybe [DisableFastSnapshotRestoreSuccessItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'disableFastSnapshotRestoresResponse_unsuccessful' - Information about the snapshots for which fast snapshot restores could
-- not be disabled.
--
-- 'successful', 'disableFastSnapshotRestoresResponse_successful' - Information about the snapshots for which fast snapshot restores were
-- successfully disabled.
--
-- 'httpStatus', 'disableFastSnapshotRestoresResponse_httpStatus' - The response's http status code.
newDisableFastSnapshotRestoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableFastSnapshotRestoresResponse
newDisableFastSnapshotRestoresResponse pHttpStatus_ =
  DisableFastSnapshotRestoresResponse'
    { unsuccessful =
        Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could
-- not be disabled.
disableFastSnapshotRestoresResponse_unsuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Prelude.Maybe [DisableFastSnapshotRestoreErrorItem])
disableFastSnapshotRestoresResponse_unsuccessful = Lens.lens (\DisableFastSnapshotRestoresResponse' {unsuccessful} -> unsuccessful) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {unsuccessful = a} :: DisableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the snapshots for which fast snapshot restores were
-- successfully disabled.
disableFastSnapshotRestoresResponse_successful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Prelude.Maybe [DisableFastSnapshotRestoreSuccessItem])
disableFastSnapshotRestoresResponse_successful = Lens.lens (\DisableFastSnapshotRestoresResponse' {successful} -> successful) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {successful = a} :: DisableFastSnapshotRestoresResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
disableFastSnapshotRestoresResponse_httpStatus :: Lens.Lens' DisableFastSnapshotRestoresResponse Prelude.Int
disableFastSnapshotRestoresResponse_httpStatus = Lens.lens (\DisableFastSnapshotRestoresResponse' {httpStatus} -> httpStatus) (\s@DisableFastSnapshotRestoresResponse' {} a -> s {httpStatus = a} :: DisableFastSnapshotRestoresResponse)

instance
  Prelude.NFData
    DisableFastSnapshotRestoresResponse
