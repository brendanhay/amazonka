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
-- Module      : Amazonka.ArcZonalShift.CancelZonalShift
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancel a zonal shift in Amazon Route 53 Application Recovery Controller
-- that you\'ve started for a resource in your AWS account in an AWS
-- Region.
module Amazonka.ArcZonalShift.CancelZonalShift
  ( -- * Creating a Request
    CancelZonalShift (..),
    newCancelZonalShift,

    -- * Request Lenses
    cancelZonalShift_zonalShiftId,

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

-- | /See:/ 'newCancelZonalShift' smart constructor.
data CancelZonalShift = CancelZonalShift'
  { -- | The internally-generated identifier of a zonal shift.
    zonalShiftId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelZonalShift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zonalShiftId', 'cancelZonalShift_zonalShiftId' - The internally-generated identifier of a zonal shift.
newCancelZonalShift ::
  -- | 'zonalShiftId'
  Prelude.Text ->
  CancelZonalShift
newCancelZonalShift pZonalShiftId_ =
  CancelZonalShift' {zonalShiftId = pZonalShiftId_}

-- | The internally-generated identifier of a zonal shift.
cancelZonalShift_zonalShiftId :: Lens.Lens' CancelZonalShift Prelude.Text
cancelZonalShift_zonalShiftId = Lens.lens (\CancelZonalShift' {zonalShiftId} -> zonalShiftId) (\s@CancelZonalShift' {} a -> s {zonalShiftId = a} :: CancelZonalShift)

instance Core.AWSRequest CancelZonalShift where
  type AWSResponse CancelZonalShift = ZonalShift
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CancelZonalShift where
  hashWithSalt _salt CancelZonalShift' {..} =
    _salt `Prelude.hashWithSalt` zonalShiftId

instance Prelude.NFData CancelZonalShift where
  rnf CancelZonalShift' {..} = Prelude.rnf zonalShiftId

instance Data.ToHeaders CancelZonalShift where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelZonalShift where
  toPath CancelZonalShift' {..} =
    Prelude.mconcat
      ["/zonalshifts/", Data.toBS zonalShiftId]

instance Data.ToQuery CancelZonalShift where
  toQuery = Prelude.const Prelude.mempty
