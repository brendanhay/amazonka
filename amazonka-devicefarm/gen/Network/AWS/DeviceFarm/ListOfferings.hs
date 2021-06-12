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
-- Module      : Network.AWS.DeviceFarm.ListOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of products or offerings that the user can manage through
-- the API. Each offering record indicates the recurring price per unit and
-- the frequency for that offering. The API returns a @NotEligible@ error
-- if the user is not permitted to invoke the operation. If you must be
-- able to invoke this operation, contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferings
  ( -- * Creating a Request
    ListOfferings (..),
    newListOfferings,

    -- * Request Lenses
    listOfferings_nextToken,

    -- * Destructuring the Response
    ListOfferingsResponse (..),
    newListOfferingsResponse,

    -- * Response Lenses
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list all offerings.
--
-- /See:/ 'newListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferings_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListOfferings ::
  ListOfferings
newListOfferings =
  ListOfferings' {nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferings_nextToken :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
listOfferings_nextToken = Lens.lens (\ListOfferings' {nextToken} -> nextToken) (\s@ListOfferings' {} a -> s {nextToken = a} :: ListOfferings)

instance Core.AWSPager ListOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_offerings Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOfferings_nextToken
          Lens..~ rs
          Lens.^? listOfferingsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOfferings where
  type
    AWSResponse ListOfferings =
      ListOfferingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "offerings" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOfferings

instance Core.NFData ListOfferings

instance Core.ToHeaders ListOfferings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListOfferings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOfferings where
  toJSON ListOfferings' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListOfferings where
  toPath = Core.const "/"

instance Core.ToQuery ListOfferings where
  toQuery = Core.const Core.mempty

-- | Represents the return values of the list of offerings.
--
-- /See:/ 'newListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | A value that represents the list offering results.
    offerings :: Core.Maybe [Offering],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'offerings', 'listOfferingsResponse_offerings' - A value that represents the list offering results.
--
-- 'httpStatus', 'listOfferingsResponse_httpStatus' - The response's http status code.
newListOfferingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOfferingsResponse
newListOfferingsResponse pHttpStatus_ =
  ListOfferingsResponse'
    { nextToken = Core.Nothing,
      offerings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingsResponse_nextToken :: Lens.Lens' ListOfferingsResponse (Core.Maybe Core.Text)
listOfferingsResponse_nextToken = Lens.lens (\ListOfferingsResponse' {nextToken} -> nextToken) (\s@ListOfferingsResponse' {} a -> s {nextToken = a} :: ListOfferingsResponse)

-- | A value that represents the list offering results.
listOfferingsResponse_offerings :: Lens.Lens' ListOfferingsResponse (Core.Maybe [Offering])
listOfferingsResponse_offerings = Lens.lens (\ListOfferingsResponse' {offerings} -> offerings) (\s@ListOfferingsResponse' {} a -> s {offerings = a} :: ListOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOfferingsResponse_httpStatus :: Lens.Lens' ListOfferingsResponse Core.Int
listOfferingsResponse_httpStatus = Lens.lens (\ListOfferingsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingsResponse' {} a -> s {httpStatus = a} :: ListOfferingsResponse)

instance Core.NFData ListOfferingsResponse
