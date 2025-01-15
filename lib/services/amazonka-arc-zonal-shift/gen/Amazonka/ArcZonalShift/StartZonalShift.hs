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
-- Module      : Amazonka.ArcZonalShift.StartZonalShift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You start a zonal shift to temporarily move load balancer traffic away
-- from an Availability Zone in a AWS Region, to help your application
-- recover immediately, for example, from a developer\'s bad code
-- deployment or from an AWS infrastructure failure in a single
-- Availability Zone. You can start a zonal shift in Route 53 ARC only for
-- managed resources in your account in an AWS Region. Resources are
-- automatically registered with Route 53 ARC by AWS services.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
--
-- When you start a zonal shift, traffic for the resource is no longer
-- routed to the Availability Zone. The zonal shift is created immediately
-- in Route 53 ARC. However, it can take a short time, typically up to a
-- few minutes, for existing, in-progress connections in the Availability
-- Zone to complete.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/arc-zonal-shift.html Zonal shift>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
module Amazonka.ArcZonalShift.StartZonalShift
  ( -- * Creating a Request
    StartZonalShift (..),
    newStartZonalShift,

    -- * Request Lenses
    startZonalShift_awayFrom,
    startZonalShift_comment,
    startZonalShift_expiresIn,
    startZonalShift_resourceIdentifier,

    -- * Destructuring the Response
    ZonalShift (..),
    newZonalShift,

    -- * Response Lenses
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,
  )
where

import Amazonka.ArcZonalShift.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartZonalShift' smart constructor.
data StartZonalShift = StartZonalShift'
  { -- | The Availability Zone that traffic is moved away from for a resource
    -- when you start a zonal shift. Until the zonal shift expires or you
    -- cancel it, traffic for the resource is instead moved to other
    -- Availability Zones in the AWS Region.
    awayFrom :: Prelude.Text,
    -- | A comment that you enter about the zonal shift. Only the latest comment
    -- is retained; no comment history is maintained. A new comment overwrites
    -- any existing comment string.
    comment :: Prelude.Text,
    -- | The length of time that you want a zonal shift to be active, which Route
    -- 53 ARC converts to an expiry time (expiration time). Zonal shifts are
    -- temporary. You can set a zonal shift to be active initially for up to
    -- three days (72 hours).
    --
    -- If you want to still keep traffic away from an Availability Zone, you
    -- can update the zonal shift and set a new expiration. You can also cancel
    -- a zonal shift, before it expires, for example, if you\'re ready to
    -- restore traffic to the Availability Zone.
    --
    -- To set a length of time for a zonal shift to be active, specify a whole
    -- number, and then one of the following, with no space:
    --
    -- >  <ul> <li> <p> <b>A lowercase letter m:</b> To specify that the value is in minutes.</p> </li> <li> <p> <b>A lowercase letter h:</b> To specify that the value is in hours.</p> </li> </ul> <p>For example: <code>20h</code> means the zonal shift expires in 20 hours. <code>120m</code> means the zonal shift expires in 120 minutes (2 hours).</p>
    expiresIn :: Prelude.Text,
    -- | The identifier for the resource to include in a zonal shift. The
    -- identifier is the Amazon Resource Name (ARN) for the resource.
    --
    -- At this time, you can only start a zonal shift for Network Load
    -- Balancers and Application Load Balancers with cross-zone load balancing
    -- turned off.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartZonalShift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awayFrom', 'startZonalShift_awayFrom' - The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
--
-- 'comment', 'startZonalShift_comment' - A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
--
-- 'expiresIn', 'startZonalShift_expiresIn' - The length of time that you want a zonal shift to be active, which Route
-- 53 ARC converts to an expiry time (expiration time). Zonal shifts are
-- temporary. You can set a zonal shift to be active initially for up to
-- three days (72 hours).
--
-- If you want to still keep traffic away from an Availability Zone, you
-- can update the zonal shift and set a new expiration. You can also cancel
-- a zonal shift, before it expires, for example, if you\'re ready to
-- restore traffic to the Availability Zone.
--
-- To set a length of time for a zonal shift to be active, specify a whole
-- number, and then one of the following, with no space:
--
-- >  <ul> <li> <p> <b>A lowercase letter m:</b> To specify that the value is in minutes.</p> </li> <li> <p> <b>A lowercase letter h:</b> To specify that the value is in hours.</p> </li> </ul> <p>For example: <code>20h</code> means the zonal shift expires in 20 hours. <code>120m</code> means the zonal shift expires in 120 minutes (2 hours).</p>
--
-- 'resourceIdentifier', 'startZonalShift_resourceIdentifier' - The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
newStartZonalShift ::
  -- | 'awayFrom'
  Prelude.Text ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'expiresIn'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  StartZonalShift
newStartZonalShift
  pAwayFrom_
  pComment_
  pExpiresIn_
  pResourceIdentifier_ =
    StartZonalShift'
      { awayFrom = pAwayFrom_,
        comment = pComment_,
        expiresIn = pExpiresIn_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The Availability Zone that traffic is moved away from for a resource
-- when you start a zonal shift. Until the zonal shift expires or you
-- cancel it, traffic for the resource is instead moved to other
-- Availability Zones in the AWS Region.
startZonalShift_awayFrom :: Lens.Lens' StartZonalShift Prelude.Text
startZonalShift_awayFrom = Lens.lens (\StartZonalShift' {awayFrom} -> awayFrom) (\s@StartZonalShift' {} a -> s {awayFrom = a} :: StartZonalShift)

-- | A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
startZonalShift_comment :: Lens.Lens' StartZonalShift Prelude.Text
startZonalShift_comment = Lens.lens (\StartZonalShift' {comment} -> comment) (\s@StartZonalShift' {} a -> s {comment = a} :: StartZonalShift)

-- | The length of time that you want a zonal shift to be active, which Route
-- 53 ARC converts to an expiry time (expiration time). Zonal shifts are
-- temporary. You can set a zonal shift to be active initially for up to
-- three days (72 hours).
--
-- If you want to still keep traffic away from an Availability Zone, you
-- can update the zonal shift and set a new expiration. You can also cancel
-- a zonal shift, before it expires, for example, if you\'re ready to
-- restore traffic to the Availability Zone.
--
-- To set a length of time for a zonal shift to be active, specify a whole
-- number, and then one of the following, with no space:
--
-- >  <ul> <li> <p> <b>A lowercase letter m:</b> To specify that the value is in minutes.</p> </li> <li> <p> <b>A lowercase letter h:</b> To specify that the value is in hours.</p> </li> </ul> <p>For example: <code>20h</code> means the zonal shift expires in 20 hours. <code>120m</code> means the zonal shift expires in 120 minutes (2 hours).</p>
startZonalShift_expiresIn :: Lens.Lens' StartZonalShift Prelude.Text
startZonalShift_expiresIn = Lens.lens (\StartZonalShift' {expiresIn} -> expiresIn) (\s@StartZonalShift' {} a -> s {expiresIn = a} :: StartZonalShift)

-- | The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
startZonalShift_resourceIdentifier :: Lens.Lens' StartZonalShift Prelude.Text
startZonalShift_resourceIdentifier = Lens.lens (\StartZonalShift' {resourceIdentifier} -> resourceIdentifier) (\s@StartZonalShift' {} a -> s {resourceIdentifier = a} :: StartZonalShift)

instance Core.AWSRequest StartZonalShift where
  type AWSResponse StartZonalShift = ZonalShift
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable StartZonalShift where
  hashWithSalt _salt StartZonalShift' {..} =
    _salt
      `Prelude.hashWithSalt` awayFrom
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` expiresIn
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData StartZonalShift where
  rnf StartZonalShift' {..} =
    Prelude.rnf awayFrom `Prelude.seq`
      Prelude.rnf comment `Prelude.seq`
        Prelude.rnf expiresIn `Prelude.seq`
          Prelude.rnf resourceIdentifier

instance Data.ToHeaders StartZonalShift where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartZonalShift where
  toJSON StartZonalShift' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("awayFrom" Data..= awayFrom),
            Prelude.Just ("comment" Data..= comment),
            Prelude.Just ("expiresIn" Data..= expiresIn),
            Prelude.Just
              ("resourceIdentifier" Data..= resourceIdentifier)
          ]
      )

instance Data.ToPath StartZonalShift where
  toPath = Prelude.const "/zonalshifts"

instance Data.ToQuery StartZonalShift where
  toQuery = Prelude.const Prelude.mempty
