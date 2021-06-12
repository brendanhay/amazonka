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
-- Module      : Network.AWS.DeviceFarm.ListOfferingPromotions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of offering promotions. Each offering promotion record
-- contains the ID and description of the promotion. The API returns a
-- @NotEligible@ error if the caller is not permitted to invoke the
-- operation. Contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>
-- if you must be able to invoke this operation.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferingPromotions
  ( -- * Creating a Request
    ListOfferingPromotions (..),
    newListOfferingPromotions,

    -- * Request Lenses
    listOfferingPromotions_nextToken,

    -- * Destructuring the Response
    ListOfferingPromotionsResponse (..),
    newListOfferingPromotionsResponse,

    -- * Response Lenses
    listOfferingPromotionsResponse_nextToken,
    listOfferingPromotionsResponse_offeringPromotions,
    listOfferingPromotionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOfferingPromotions' smart constructor.
data ListOfferingPromotions = ListOfferingPromotions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferingPromotions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingPromotions_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListOfferingPromotions ::
  ListOfferingPromotions
newListOfferingPromotions =
  ListOfferingPromotions' {nextToken = Core.Nothing}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingPromotions_nextToken :: Lens.Lens' ListOfferingPromotions (Core.Maybe Core.Text)
listOfferingPromotions_nextToken = Lens.lens (\ListOfferingPromotions' {nextToken} -> nextToken) (\s@ListOfferingPromotions' {} a -> s {nextToken = a} :: ListOfferingPromotions)

instance Core.AWSPager ListOfferingPromotions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingPromotionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingPromotionsResponse_offeringPromotions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOfferingPromotions_nextToken
          Lens..~ rs
          Lens.^? listOfferingPromotionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListOfferingPromotions where
  type
    AWSResponse ListOfferingPromotions =
      ListOfferingPromotionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingPromotionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "offeringPromotions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOfferingPromotions

instance Core.NFData ListOfferingPromotions

instance Core.ToHeaders ListOfferingPromotions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListOfferingPromotions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOfferingPromotions where
  toJSON ListOfferingPromotions' {..} =
    Core.object
      ( Core.catMaybes
          [("nextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListOfferingPromotions where
  toPath = Core.const "/"

instance Core.ToQuery ListOfferingPromotions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListOfferingPromotionsResponse' smart constructor.
data ListOfferingPromotionsResponse = ListOfferingPromotionsResponse'
  { -- | An identifier to be used in the next call to this operation, to return
    -- the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the offering promotions.
    offeringPromotions :: Core.Maybe [OfferingPromotion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOfferingPromotionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingPromotionsResponse_nextToken' - An identifier to be used in the next call to this operation, to return
-- the next set of items in the list.
--
-- 'offeringPromotions', 'listOfferingPromotionsResponse_offeringPromotions' - Information about the offering promotions.
--
-- 'httpStatus', 'listOfferingPromotionsResponse_httpStatus' - The response's http status code.
newListOfferingPromotionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOfferingPromotionsResponse
newListOfferingPromotionsResponse pHttpStatus_ =
  ListOfferingPromotionsResponse'
    { nextToken =
        Core.Nothing,
      offeringPromotions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be used in the next call to this operation, to return
-- the next set of items in the list.
listOfferingPromotionsResponse_nextToken :: Lens.Lens' ListOfferingPromotionsResponse (Core.Maybe Core.Text)
listOfferingPromotionsResponse_nextToken = Lens.lens (\ListOfferingPromotionsResponse' {nextToken} -> nextToken) (\s@ListOfferingPromotionsResponse' {} a -> s {nextToken = a} :: ListOfferingPromotionsResponse)

-- | Information about the offering promotions.
listOfferingPromotionsResponse_offeringPromotions :: Lens.Lens' ListOfferingPromotionsResponse (Core.Maybe [OfferingPromotion])
listOfferingPromotionsResponse_offeringPromotions = Lens.lens (\ListOfferingPromotionsResponse' {offeringPromotions} -> offeringPromotions) (\s@ListOfferingPromotionsResponse' {} a -> s {offeringPromotions = a} :: ListOfferingPromotionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOfferingPromotionsResponse_httpStatus :: Lens.Lens' ListOfferingPromotionsResponse Core.Int
listOfferingPromotionsResponse_httpStatus = Lens.lens (\ListOfferingPromotionsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingPromotionsResponse' {} a -> s {httpStatus = a} :: ListOfferingPromotionsResponse)

instance Core.NFData ListOfferingPromotionsResponse
