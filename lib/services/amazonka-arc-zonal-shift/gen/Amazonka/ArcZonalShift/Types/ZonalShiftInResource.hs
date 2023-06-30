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
-- Module      : Amazonka.ArcZonalShift.Types.ZonalShiftInResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Types.ZonalShiftInResource where

import Amazonka.ArcZonalShift.Types.AppliedStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex structure that lists the zonal shifts for a managed resource
-- and their statuses for the resource.
--
-- /See:/ 'newZonalShiftInResource' smart constructor.
data ZonalShiftInResource = ZonalShiftInResource'
  { -- | An @appliedStatus@ for a zonal shift for a resource can have one of two
    -- values: @APPLIED@ or @NOT_APPLIED@.
    appliedStatus :: AppliedStatus,
    -- | The Availability Zone that traffic is moved away from for a resource
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
    -- | The identifier of a zonal shift.
    zonalShiftId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZonalShiftInResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedStatus', 'zonalShiftInResource_appliedStatus' - An @appliedStatus@ for a zonal shift for a resource can have one of two
-- values: @APPLIED@ or @NOT_APPLIED@.
--
-- 'awayFrom', 'zonalShiftInResource_awayFrom' - The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
--
-- 'comment', 'zonalShiftInResource_comment' - A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. That is, a new comment
-- overwrites any existing comment string.
--
-- 'expiryTime', 'zonalShiftInResource_expiryTime' - The expiry time (expiration time) for the zonal shift. A zonal shift is
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
-- 'resourceIdentifier', 'zonalShiftInResource_resourceIdentifier' - The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
--
-- 'startTime', 'zonalShiftInResource_startTime' - The time (UTC) when the zonal shift is started.
--
-- 'zonalShiftId', 'zonalShiftInResource_zonalShiftId' - The identifier of a zonal shift.
newZonalShiftInResource ::
  -- | 'appliedStatus'
  AppliedStatus ->
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
  -- | 'zonalShiftId'
  Prelude.Text ->
  ZonalShiftInResource
newZonalShiftInResource
  pAppliedStatus_
  pAwayFrom_
  pComment_
  pExpiryTime_
  pResourceIdentifier_
  pStartTime_
  pZonalShiftId_ =
    ZonalShiftInResource'
      { appliedStatus =
          pAppliedStatus_,
        awayFrom = pAwayFrom_,
        comment = pComment_,
        expiryTime = Data._Time Lens.# pExpiryTime_,
        resourceIdentifier = pResourceIdentifier_,
        startTime = Data._Time Lens.# pStartTime_,
        zonalShiftId = pZonalShiftId_
      }

-- | An @appliedStatus@ for a zonal shift for a resource can have one of two
-- values: @APPLIED@ or @NOT_APPLIED@.
zonalShiftInResource_appliedStatus :: Lens.Lens' ZonalShiftInResource AppliedStatus
zonalShiftInResource_appliedStatus = Lens.lens (\ZonalShiftInResource' {appliedStatus} -> appliedStatus) (\s@ZonalShiftInResource' {} a -> s {appliedStatus = a} :: ZonalShiftInResource)

-- | The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
zonalShiftInResource_awayFrom :: Lens.Lens' ZonalShiftInResource Prelude.Text
zonalShiftInResource_awayFrom = Lens.lens (\ZonalShiftInResource' {awayFrom} -> awayFrom) (\s@ZonalShiftInResource' {} a -> s {awayFrom = a} :: ZonalShiftInResource)

-- | A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. That is, a new comment
-- overwrites any existing comment string.
zonalShiftInResource_comment :: Lens.Lens' ZonalShiftInResource Prelude.Text
zonalShiftInResource_comment = Lens.lens (\ZonalShiftInResource' {comment} -> comment) (\s@ZonalShiftInResource' {} a -> s {comment = a} :: ZonalShiftInResource)

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
zonalShiftInResource_expiryTime :: Lens.Lens' ZonalShiftInResource Prelude.UTCTime
zonalShiftInResource_expiryTime = Lens.lens (\ZonalShiftInResource' {expiryTime} -> expiryTime) (\s@ZonalShiftInResource' {} a -> s {expiryTime = a} :: ZonalShiftInResource) Prelude.. Data._Time

-- | The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
zonalShiftInResource_resourceIdentifier :: Lens.Lens' ZonalShiftInResource Prelude.Text
zonalShiftInResource_resourceIdentifier = Lens.lens (\ZonalShiftInResource' {resourceIdentifier} -> resourceIdentifier) (\s@ZonalShiftInResource' {} a -> s {resourceIdentifier = a} :: ZonalShiftInResource)

-- | The time (UTC) when the zonal shift is started.
zonalShiftInResource_startTime :: Lens.Lens' ZonalShiftInResource Prelude.UTCTime
zonalShiftInResource_startTime = Lens.lens (\ZonalShiftInResource' {startTime} -> startTime) (\s@ZonalShiftInResource' {} a -> s {startTime = a} :: ZonalShiftInResource) Prelude.. Data._Time

-- | The identifier of a zonal shift.
zonalShiftInResource_zonalShiftId :: Lens.Lens' ZonalShiftInResource Prelude.Text
zonalShiftInResource_zonalShiftId = Lens.lens (\ZonalShiftInResource' {zonalShiftId} -> zonalShiftId) (\s@ZonalShiftInResource' {} a -> s {zonalShiftId = a} :: ZonalShiftInResource)

instance Data.FromJSON ZonalShiftInResource where
  parseJSON =
    Data.withObject
      "ZonalShiftInResource"
      ( \x ->
          ZonalShiftInResource'
            Prelude.<$> (x Data..: "appliedStatus")
            Prelude.<*> (x Data..: "awayFrom")
            Prelude.<*> (x Data..: "comment")
            Prelude.<*> (x Data..: "expiryTime")
            Prelude.<*> (x Data..: "resourceIdentifier")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..: "zonalShiftId")
      )

instance Prelude.Hashable ZonalShiftInResource where
  hashWithSalt _salt ZonalShiftInResource' {..} =
    _salt
      `Prelude.hashWithSalt` appliedStatus
      `Prelude.hashWithSalt` awayFrom
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` expiryTime
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` zonalShiftId

instance Prelude.NFData ZonalShiftInResource where
  rnf ZonalShiftInResource' {..} =
    Prelude.rnf appliedStatus
      `Prelude.seq` Prelude.rnf awayFrom
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf expiryTime
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf zonalShiftId
