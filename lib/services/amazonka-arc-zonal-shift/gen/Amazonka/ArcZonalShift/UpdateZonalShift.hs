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
-- Module      : Amazonka.ArcZonalShift.UpdateZonalShift
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an active zonal shift in Amazon Route 53 Application Recovery
-- Controller in your AWS account. You can update a zonal shift to set a
-- new expiration, or edit or replace the comment for the zonal shift.
module Amazonka.ArcZonalShift.UpdateZonalShift
  ( -- * Creating a Request
    UpdateZonalShift (..),
    newUpdateZonalShift,

    -- * Request Lenses
    updateZonalShift_comment,
    updateZonalShift_expiresIn,
    updateZonalShift_zonalShiftId,

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

-- | /See:/ 'newUpdateZonalShift' smart constructor.
data UpdateZonalShift = UpdateZonalShift'
  { -- | A comment that you enter about the zonal shift. Only the latest comment
    -- is retained; no comment history is maintained. A new comment overwrites
    -- any existing comment string.
    comment :: Prelude.Maybe Prelude.Text,
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
    -- -   __A lowercase letter m:__ To specify that the value is in minutes.
    --
    -- -   __A lowercase letter h:__ To specify that the value is in hours.
    --
    -- For example: @20h@ means the zonal shift expires in 20 hours. @120m@
    -- means the zonal shift expires in 120 minutes (2 hours).
    expiresIn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a zonal shift.
    zonalShiftId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateZonalShift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'updateZonalShift_comment' - A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
--
-- 'expiresIn', 'updateZonalShift_expiresIn' - The length of time that you want a zonal shift to be active, which Route
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
-- -   __A lowercase letter m:__ To specify that the value is in minutes.
--
-- -   __A lowercase letter h:__ To specify that the value is in hours.
--
-- For example: @20h@ means the zonal shift expires in 20 hours. @120m@
-- means the zonal shift expires in 120 minutes (2 hours).
--
-- 'zonalShiftId', 'updateZonalShift_zonalShiftId' - The identifier of a zonal shift.
newUpdateZonalShift ::
  -- | 'zonalShiftId'
  Prelude.Text ->
  UpdateZonalShift
newUpdateZonalShift pZonalShiftId_ =
  UpdateZonalShift'
    { comment = Prelude.Nothing,
      expiresIn = Prelude.Nothing,
      zonalShiftId = pZonalShiftId_
    }

-- | A comment that you enter about the zonal shift. Only the latest comment
-- is retained; no comment history is maintained. A new comment overwrites
-- any existing comment string.
updateZonalShift_comment :: Lens.Lens' UpdateZonalShift (Prelude.Maybe Prelude.Text)
updateZonalShift_comment = Lens.lens (\UpdateZonalShift' {comment} -> comment) (\s@UpdateZonalShift' {} a -> s {comment = a} :: UpdateZonalShift)

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
-- -   __A lowercase letter m:__ To specify that the value is in minutes.
--
-- -   __A lowercase letter h:__ To specify that the value is in hours.
--
-- For example: @20h@ means the zonal shift expires in 20 hours. @120m@
-- means the zonal shift expires in 120 minutes (2 hours).
updateZonalShift_expiresIn :: Lens.Lens' UpdateZonalShift (Prelude.Maybe Prelude.Text)
updateZonalShift_expiresIn = Lens.lens (\UpdateZonalShift' {expiresIn} -> expiresIn) (\s@UpdateZonalShift' {} a -> s {expiresIn = a} :: UpdateZonalShift)

-- | The identifier of a zonal shift.
updateZonalShift_zonalShiftId :: Lens.Lens' UpdateZonalShift Prelude.Text
updateZonalShift_zonalShiftId = Lens.lens (\UpdateZonalShift' {zonalShiftId} -> zonalShiftId) (\s@UpdateZonalShift' {} a -> s {zonalShiftId = a} :: UpdateZonalShift)

instance Core.AWSRequest UpdateZonalShift where
  type AWSResponse UpdateZonalShift = ZonalShift
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateZonalShift where
  hashWithSalt _salt UpdateZonalShift' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` expiresIn
      `Prelude.hashWithSalt` zonalShiftId

instance Prelude.NFData UpdateZonalShift where
  rnf UpdateZonalShift' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf expiresIn
      `Prelude.seq` Prelude.rnf zonalShiftId

instance Data.ToHeaders UpdateZonalShift where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateZonalShift where
  toJSON UpdateZonalShift' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("expiresIn" Data..=) Prelude.<$> expiresIn
          ]
      )

instance Data.ToPath UpdateZonalShift where
  toPath UpdateZonalShift' {..} =
    Prelude.mconcat
      ["/zonalshifts/", Data.toBS zonalShiftId]

instance Data.ToQuery UpdateZonalShift where
  toQuery = Prelude.const Prelude.mempty
