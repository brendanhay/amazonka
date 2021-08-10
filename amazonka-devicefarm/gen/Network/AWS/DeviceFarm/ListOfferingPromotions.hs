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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOfferingPromotions' smart constructor.
data ListOfferingPromotions = ListOfferingPromotions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  ListOfferingPromotions'
    { nextToken =
        Prelude.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listOfferingPromotions_nextToken :: Lens.Lens' ListOfferingPromotions (Prelude.Maybe Prelude.Text)
listOfferingPromotions_nextToken = Lens.lens (\ListOfferingPromotions' {nextToken} -> nextToken) (\s@ListOfferingPromotions' {} a -> s {nextToken = a} :: ListOfferingPromotions)

instance Core.AWSPager ListOfferingPromotions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingPromotionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingPromotionsResponse_offeringPromotions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOfferingPromotions_nextToken
          Lens..~ rs
          Lens.^? listOfferingPromotionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOfferingPromotions where
  type
    AWSResponse ListOfferingPromotions =
      ListOfferingPromotionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingPromotionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "offeringPromotions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOfferingPromotions

instance Prelude.NFData ListOfferingPromotions

instance Core.ToHeaders ListOfferingPromotions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListOfferingPromotions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOfferingPromotions where
  toJSON ListOfferingPromotions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("nextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListOfferingPromotions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOfferingPromotions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOfferingPromotionsResponse' smart constructor.
data ListOfferingPromotionsResponse = ListOfferingPromotionsResponse'
  { -- | An identifier to be used in the next call to this operation, to return
    -- the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the offering promotions.
    offeringPromotions :: Prelude.Maybe [OfferingPromotion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListOfferingPromotionsResponse
newListOfferingPromotionsResponse pHttpStatus_ =
  ListOfferingPromotionsResponse'
    { nextToken =
        Prelude.Nothing,
      offeringPromotions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be used in the next call to this operation, to return
-- the next set of items in the list.
listOfferingPromotionsResponse_nextToken :: Lens.Lens' ListOfferingPromotionsResponse (Prelude.Maybe Prelude.Text)
listOfferingPromotionsResponse_nextToken = Lens.lens (\ListOfferingPromotionsResponse' {nextToken} -> nextToken) (\s@ListOfferingPromotionsResponse' {} a -> s {nextToken = a} :: ListOfferingPromotionsResponse)

-- | Information about the offering promotions.
listOfferingPromotionsResponse_offeringPromotions :: Lens.Lens' ListOfferingPromotionsResponse (Prelude.Maybe [OfferingPromotion])
listOfferingPromotionsResponse_offeringPromotions = Lens.lens (\ListOfferingPromotionsResponse' {offeringPromotions} -> offeringPromotions) (\s@ListOfferingPromotionsResponse' {} a -> s {offeringPromotions = a} :: ListOfferingPromotionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOfferingPromotionsResponse_httpStatus :: Lens.Lens' ListOfferingPromotionsResponse Prelude.Int
listOfferingPromotionsResponse_httpStatus = Lens.lens (\ListOfferingPromotionsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingPromotionsResponse' {} a -> s {httpStatus = a} :: ListOfferingPromotionsResponse)

instance
  Prelude.NFData
    ListOfferingPromotionsResponse
