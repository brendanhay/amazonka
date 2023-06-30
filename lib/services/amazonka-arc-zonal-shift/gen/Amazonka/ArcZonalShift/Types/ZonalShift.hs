{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ArcZonalShift.Types.ZonalShift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Types.ZonalShift where

import Amazonka.ArcZonalShift.Types.ZonalShiftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newZonalShift' smart constructor.
data ZonalShift = ZonalShift'
  { -- | The Availability Zone that traffic is moved away from for a resource
    -- when you start a zonal shift. Until the zonal shift expires or you
    -- cancel it, traffic for the resource is instead moved to other
    -- Availability Zones in the AWS Region.
    awayFrom :: Prelude.Text,
    -- | A comment that you enter about the zonal shift. Only the latest comment
    -- is retained; no comment history is maintained. A new comment overwrites
    -- any existing comment string.
    comment :: Prelude.Text,
    -- | The expiry time (expiration time) for the zonal shift. A zonal shift is
    -- temporary and must be set to expire when you start the zonal shift. You
    -- can initially set a zonal shift to expire in a maximum of three days (72
    -- hours). However, you can update a zonal shift to set a new expiration at
    -- any time.
    --
    -- When you start a zonal shift, you specify how long you want it to be
    -- active, which Route 53 ARC converts to an expiry time (expiration time).
    -- You can cancel a zonal shift, for example, if you\'re ready to restore
    -- traffic to the Availability Zone. Or you can update the zonal shift to
    -- specify another length of time to expire in.
    expiryTime :: Data.POSIX,
    -- | The identifier for the resource to include in a zonal shift. The
    -- identifier is the Amazon Resource Name (ARN) for the resource.
    --
    -- At this time, you can only start a zonal shift for Network Load
    -- Balancers and Application Load Balancers with cross-zone load balancing
    -- turned off.
    resourceIdentifier :: Prelude.Text,
    -- | The time (UTC) when the zonal shift is started.
    startTime :: Data.POSIX,
    -- | A status for a zonal shift.
    --
    -- The @Status@ for a zonal shift can have one of the following values:
    --
    -- -   __ACTIVE:__ The zonal shift is started and active.
    --
    -- -   __EXPIRED:__ The zonal shift has expired (the expiry time was
    --     exceeded).
    --
    -- -   __CANCELED:__ The zonal shift was canceled.
    status :: ZonalShiftStatus,
    -- | The identifier of a zonal shift.
    zonalShiftId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZonalShift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awayFrom', 'zonalShift_awayFrom' - The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
--
-- 'comment', 'zonalShift_comment' - A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
--
-- 'expiryTime', 'zonalShift_expiryTime' - The expiry time (expiration time) for the zonal shift. A zonal shift is
-- temporary and must be set to expire when you start the zonal shift. You
-- can initially set a zonal shift to expire in a maximum of three days (72
-- hours). However, you can update a zonal shift to set a new expiration at
-- any time.
--
-- When you start a zonal shift, you specify how long you want it to be
-- active, which Route 53 ARC converts to an expiry time (expiration time).
-- You can cancel a zonal shift, for example, if you\'re ready to restore
-- traffic to the Availability Zone. Or you can update the zonal shift to
-- specify another length of time to expire in.
--
-- 'resourceIdentifier', 'zonalShift_resourceIdentifier' - The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
--
-- 'startTime', 'zonalShift_startTime' - The time (UTC) when the zonal shift is started.
--
-- 'status', 'zonalShift_status' - A status for a zonal shift.
--
-- The @Status@ for a zonal shift can have one of the following values:
--
-- -   __ACTIVE:__ The zonal shift is started and active.
--
-- -   __EXPIRED:__ The zonal shift has expired (the expiry time was
--     exceeded).
--
-- -   __CANCELED:__ The zonal shift was canceled.
--
-- 'zonalShiftId', 'zonalShift_zonalShiftId' - The identifier of a zonal shift.
newZonalShift ::
  -- | 'awayFrom'
  Prelude.Text ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'expiryTime'
  Prelude.UTCTime ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'status'
  ZonalShiftStatus ->
  -- | 'zonalShiftId'
  Prelude.Text ->
  ZonalShift
newZonalShift
  pAwayFrom_
  pComment_
  pExpiryTime_
  pResourceIdentifier_
  pStartTime_
  pStatus_
  pZonalShiftId_ =
    ZonalShift'
      { awayFrom = pAwayFrom_,
        comment = pComment_,
        expiryTime = Data._Time Lens.# pExpiryTime_,
        resourceIdentifier = pResourceIdentifier_,
        startTime = Data._Time Lens.# pStartTime_,
        status = pStatus_,
        zonalShiftId = pZonalShiftId_
      }

-- | The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
zonalShift_awayFrom :: Lens.Lens' ZonalShift Prelude.Text
zonalShift_awayFrom = Lens.lens (\ZonalShift' {awayFrom} -> awayFrom) (\s@ZonalShift' {} a -> s {awayFrom = a} :: ZonalShift)

-- | A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
zonalShift_comment :: Lens.Lens' ZonalShift Prelude.Text
zonalShift_comment = Lens.lens (\ZonalShift' {comment} -> comment) (\s@ZonalShift' {} a -> s {comment = a} :: ZonalShift)

-- | The expiry time (expiration time) for the zonal shift. A zonal shift is
-- temporary and must be set to expire when you start the zonal shift. You
-- can initially set a zonal shift to expire in a maximum of three days (72
-- hours). However, you can update a zonal shift to set a new expiration at
-- any time.
--
-- When you start a zonal shift, you specify how long you want it to be
-- active, which Route 53 ARC converts to an expiry time (expiration time).
-- You can cancel a zonal shift, for example, if you\'re ready to restore
-- traffic to the Availability Zone. Or you can update the zonal shift to
-- specify another length of time to expire in.
zonalShift_expiryTime :: Lens.Lens' ZonalShift Prelude.UTCTime
zonalShift_expiryTime = Lens.lens (\ZonalShift' {expiryTime} -> expiryTime) (\s@ZonalShift' {} a -> s {expiryTime = a} :: ZonalShift) Prelude.. Data._Time

-- | The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
zonalShift_resourceIdentifier :: Lens.Lens' ZonalShift Prelude.Text
zonalShift_resourceIdentifier = Lens.lens (\ZonalShift' {resourceIdentifier} -> resourceIdentifier) (\s@ZonalShift' {} a -> s {resourceIdentifier = a} :: ZonalShift)

-- | The time (UTC) when the zonal shift is started.
zonalShift_startTime :: Lens.Lens' ZonalShift Prelude.UTCTime
zonalShift_startTime = Lens.lens (\ZonalShift' {startTime} -> startTime) (\s@ZonalShift' {} a -> s {startTime = a} :: ZonalShift) Prelude.. Data._Time

-- | A status for a zonal shift.
--
-- The @Status@ for a zonal shift can have one of the following values:
--
-- -   __ACTIVE:__ The zonal shift is started and active.
--
-- -   __EXPIRED:__ The zonal shift has expired (the expiry time was
--     exceeded).
--
-- -   __CANCELED:__ The zonal shift was canceled.
zonalShift_status :: Lens.Lens' ZonalShift ZonalShiftStatus
zonalShift_status = Lens.lens (\ZonalShift' {status} -> status) (\s@ZonalShift' {} a -> s {status = a} :: ZonalShift)

-- | The identifier of a zonal shift.
zonalShift_zonalShiftId :: Lens.Lens' ZonalShift Prelude.Text
zonalShift_zonalShiftId = Lens.lens (\ZonalShift' {zonalShiftId} -> zonalShiftId) (\s@ZonalShift' {} a -> s {zonalShiftId = a} :: ZonalShift)

instance Data.FromJSON ZonalShift where
  parseJSON =
    Data.withObject
      "ZonalShift"
      ( \x ->
          ZonalShift'
            Prelude.<$> (x Data..: "awayFrom")
            Prelude.<*> (x Data..: "comment")
            Prelude.<*> (x Data..: "expiryTime")
            Prelude.<*> (x Data..: "resourceIdentifier")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "zonalShiftId")
      )

instance Prelude.Hashable ZonalShift where
  hashWithSalt _salt ZonalShift' {..} =
    _salt
      `Prelude.hashWithSalt` awayFrom
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` expiryTime
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` zonalShiftId

instance Prelude.NFData ZonalShift where
  rnf ZonalShift' {..} =
    Prelude.rnf awayFrom
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf zonalShiftId
