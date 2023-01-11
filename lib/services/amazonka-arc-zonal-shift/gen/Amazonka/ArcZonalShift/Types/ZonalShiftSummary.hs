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
-- Module      : Amazonka.ArcZonalShift.Types.ZonalShiftSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Types.ZonalShiftSummary where

import Amazonka.ArcZonalShift.Types.ZonalShiftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | You start a zonal shift to temporarily move load balancer traffic away
-- from an Availability Zone in a AWS Region. A zonal shift helps your
-- application recover immediately, for example, from a developer\'s bad
-- code deployment or from an AWS infrastructure failure in a single
-- Availability Zone. You can start a zonal shift in Route 53 ARC only for
-- managed resources in your account in an AWS Region. Supported AWS
-- resources are automatically registered with Route 53 ARC.
--
-- Zonal shifts are temporary. A zonal shift can be active for up to three
-- days (72 hours).
--
-- When you start a zonal shift, you specify how long you want it to be
-- active, which Amazon Route 53 Application Recovery Controller converts
-- to an expiry time (expiration time). You can cancel a zonal shift, for
-- example, if you\'re ready to restore traffic to the Availability Zone.
-- Or you can extend the zonal shift by updating the expiration so the
-- zonal shift is active longer.
--
-- /See:/ 'newZonalShiftSummary' smart constructor.
data ZonalShiftSummary = ZonalShiftSummary'
  { -- | The Availability Zone that traffic is moved away from for a resource
    -- when you start a zonal shift. Until the zonal shift expires or you
    -- cancel it, traffic for the resource is instead moved to other
    -- Availability Zones in the AWS Region.
    awayFrom :: Prelude.Text,
    -- | A comment that you enter about the zonal shift. Only the latest comment
    -- is retained; no comment history is maintained. That is, a new comment
    -- overwrites any existing comment string.
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
-- Create a value of 'ZonalShiftSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awayFrom', 'zonalShiftSummary_awayFrom' - The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
--
-- 'comment', 'zonalShiftSummary_comment' - A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. That is, a new comment
-- overwrites any existing comment string.
--
-- 'expiryTime', 'zonalShiftSummary_expiryTime' - The expiry time (expiration time) for the zonal shift. A zonal shift is
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
-- 'resourceIdentifier', 'zonalShiftSummary_resourceIdentifier' - The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
--
-- 'startTime', 'zonalShiftSummary_startTime' - The time (UTC) when the zonal shift is started.
--
-- 'status', 'zonalShiftSummary_status' - A status for a zonal shift.
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
-- 'zonalShiftId', 'zonalShiftSummary_zonalShiftId' - The identifier of a zonal shift.
newZonalShiftSummary ::
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
  ZonalShiftSummary
newZonalShiftSummary
  pAwayFrom_
  pComment_
  pExpiryTime_
  pResourceIdentifier_
  pStartTime_
  pStatus_
  pZonalShiftId_ =
    ZonalShiftSummary'
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
zonalShiftSummary_awayFrom :: Lens.Lens' ZonalShiftSummary Prelude.Text
zonalShiftSummary_awayFrom = Lens.lens (\ZonalShiftSummary' {awayFrom} -> awayFrom) (\s@ZonalShiftSummary' {} a -> s {awayFrom = a} :: ZonalShiftSummary)

-- | A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. That is, a new comment
-- overwrites any existing comment string.
zonalShiftSummary_comment :: Lens.Lens' ZonalShiftSummary Prelude.Text
zonalShiftSummary_comment = Lens.lens (\ZonalShiftSummary' {comment} -> comment) (\s@ZonalShiftSummary' {} a -> s {comment = a} :: ZonalShiftSummary)

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
zonalShiftSummary_expiryTime :: Lens.Lens' ZonalShiftSummary Prelude.UTCTime
zonalShiftSummary_expiryTime = Lens.lens (\ZonalShiftSummary' {expiryTime} -> expiryTime) (\s@ZonalShiftSummary' {} a -> s {expiryTime = a} :: ZonalShiftSummary) Prelude.. Data._Time

-- | The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
zonalShiftSummary_resourceIdentifier :: Lens.Lens' ZonalShiftSummary Prelude.Text
zonalShiftSummary_resourceIdentifier = Lens.lens (\ZonalShiftSummary' {resourceIdentifier} -> resourceIdentifier) (\s@ZonalShiftSummary' {} a -> s {resourceIdentifier = a} :: ZonalShiftSummary)

-- | The time (UTC) when the zonal shift is started.
zonalShiftSummary_startTime :: Lens.Lens' ZonalShiftSummary Prelude.UTCTime
zonalShiftSummary_startTime = Lens.lens (\ZonalShiftSummary' {startTime} -> startTime) (\s@ZonalShiftSummary' {} a -> s {startTime = a} :: ZonalShiftSummary) Prelude.. Data._Time

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
zonalShiftSummary_status :: Lens.Lens' ZonalShiftSummary ZonalShiftStatus
zonalShiftSummary_status = Lens.lens (\ZonalShiftSummary' {status} -> status) (\s@ZonalShiftSummary' {} a -> s {status = a} :: ZonalShiftSummary)

-- | The identifier of a zonal shift.
zonalShiftSummary_zonalShiftId :: Lens.Lens' ZonalShiftSummary Prelude.Text
zonalShiftSummary_zonalShiftId = Lens.lens (\ZonalShiftSummary' {zonalShiftId} -> zonalShiftId) (\s@ZonalShiftSummary' {} a -> s {zonalShiftId = a} :: ZonalShiftSummary)

instance Data.FromJSON ZonalShiftSummary where
  parseJSON =
    Data.withObject
      "ZonalShiftSummary"
      ( \x ->
          ZonalShiftSummary'
            Prelude.<$> (x Data..: "awayFrom")
            Prelude.<*> (x Data..: "comment")
            Prelude.<*> (x Data..: "expiryTime")
            Prelude.<*> (x Data..: "resourceIdentifier")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "zonalShiftId")
      )

instance Prelude.Hashable ZonalShiftSummary where
  hashWithSalt _salt ZonalShiftSummary' {..} =
    _salt `Prelude.hashWithSalt` awayFrom
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` expiryTime
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` zonalShiftId

instance Prelude.NFData ZonalShiftSummary where
  rnf ZonalShiftSummary' {..} =
    Prelude.rnf awayFrom
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf zonalShiftId
