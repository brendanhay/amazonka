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
-- Module      : Network.AWS.DeviceFarm.GetOfferingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current status and future status of all offerings purchased by
-- an AWS account. The response indicates how many offerings are currently
-- available and the offerings that will be available in the next period.
-- The API returns a @NotEligible@ error if the user is not permitted to
-- invoke the operation. If you must be able to invoke this operation,
-- contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.GetOfferingStatus
  ( -- * Creating a Request
    GetOfferingStatus (..),
    newGetOfferingStatus,

    -- * Request Lenses
    getOfferingStatus_nextToken,

    -- * Destructuring the Response
    GetOfferingStatusResponse (..),
    newGetOfferingStatusResponse,

    -- * Response Lenses
    getOfferingStatusResponse_nextToken,
    getOfferingStatusResponse_nextPeriod,
    getOfferingStatusResponse_current,
    getOfferingStatusResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to retrieve the offering status for the specified
-- customer or account.
--
-- /See:/ 'newGetOfferingStatus' smart constructor.
data GetOfferingStatus = GetOfferingStatus'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetOfferingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOfferingStatus_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newGetOfferingStatus ::
  GetOfferingStatus
newGetOfferingStatus =
  GetOfferingStatus' {nextToken = Prelude.Nothing}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
getOfferingStatus_nextToken :: Lens.Lens' GetOfferingStatus (Prelude.Maybe Prelude.Text)
getOfferingStatus_nextToken = Lens.lens (\GetOfferingStatus' {nextToken} -> nextToken) (\s@GetOfferingStatus' {} a -> s {nextToken = a} :: GetOfferingStatus)

instance Pager.AWSPager GetOfferingStatus where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getOfferingStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getOfferingStatusResponse_current
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getOfferingStatusResponse_nextPeriod
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getOfferingStatus_nextToken
          Lens..~ rs
          Lens.^? getOfferingStatusResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetOfferingStatus where
  type Rs GetOfferingStatus = GetOfferingStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOfferingStatusResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "nextPeriod"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "current" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOfferingStatus

instance Prelude.NFData GetOfferingStatus

instance Prelude.ToHeaders GetOfferingStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.GetOfferingStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetOfferingStatus where
  toJSON GetOfferingStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("nextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath GetOfferingStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetOfferingStatus where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the status result for a device offering.
--
-- /See:/ 'newGetOfferingStatusResponse' smart constructor.
data GetOfferingStatusResponse = GetOfferingStatusResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, gets the offering status for the next period.
    nextPeriod :: Prelude.Maybe (Prelude.HashMap Prelude.Text OfferingStatus),
    -- | When specified, gets the offering status for the current period.
    current :: Prelude.Maybe (Prelude.HashMap Prelude.Text OfferingStatus),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetOfferingStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOfferingStatusResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'nextPeriod', 'getOfferingStatusResponse_nextPeriod' - When specified, gets the offering status for the next period.
--
-- 'current', 'getOfferingStatusResponse_current' - When specified, gets the offering status for the current period.
--
-- 'httpStatus', 'getOfferingStatusResponse_httpStatus' - The response's http status code.
newGetOfferingStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOfferingStatusResponse
newGetOfferingStatusResponse pHttpStatus_ =
  GetOfferingStatusResponse'
    { nextToken =
        Prelude.Nothing,
      nextPeriod = Prelude.Nothing,
      current = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
getOfferingStatusResponse_nextToken :: Lens.Lens' GetOfferingStatusResponse (Prelude.Maybe Prelude.Text)
getOfferingStatusResponse_nextToken = Lens.lens (\GetOfferingStatusResponse' {nextToken} -> nextToken) (\s@GetOfferingStatusResponse' {} a -> s {nextToken = a} :: GetOfferingStatusResponse)

-- | When specified, gets the offering status for the next period.
getOfferingStatusResponse_nextPeriod :: Lens.Lens' GetOfferingStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text OfferingStatus))
getOfferingStatusResponse_nextPeriod = Lens.lens (\GetOfferingStatusResponse' {nextPeriod} -> nextPeriod) (\s@GetOfferingStatusResponse' {} a -> s {nextPeriod = a} :: GetOfferingStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | When specified, gets the offering status for the current period.
getOfferingStatusResponse_current :: Lens.Lens' GetOfferingStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text OfferingStatus))
getOfferingStatusResponse_current = Lens.lens (\GetOfferingStatusResponse' {current} -> current) (\s@GetOfferingStatusResponse' {} a -> s {current = a} :: GetOfferingStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getOfferingStatusResponse_httpStatus :: Lens.Lens' GetOfferingStatusResponse Prelude.Int
getOfferingStatusResponse_httpStatus = Lens.lens (\GetOfferingStatusResponse' {httpStatus} -> httpStatus) (\s@GetOfferingStatusResponse' {} a -> s {httpStatus = a} :: GetOfferingStatusResponse)

instance Prelude.NFData GetOfferingStatusResponse
