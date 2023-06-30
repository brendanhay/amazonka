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
-- Module      : Amazonka.AlexaBusiness.ListSmartHomeAppliances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the smart home appliances associated with a room.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListSmartHomeAppliances
  ( -- * Creating a Request
    ListSmartHomeAppliances (..),
    newListSmartHomeAppliances,

    -- * Request Lenses
    listSmartHomeAppliances_maxResults,
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_roomArn,

    -- * Destructuring the Response
    ListSmartHomeAppliancesResponse (..),
    newListSmartHomeAppliancesResponse,

    -- * Response Lenses
    listSmartHomeAppliancesResponse_nextToken,
    listSmartHomeAppliancesResponse_smartHomeAppliances,
    listSmartHomeAppliancesResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSmartHomeAppliances' smart constructor.
data ListSmartHomeAppliances = ListSmartHomeAppliances'
  { -- | The maximum number of appliances to be returned, per paginated calls.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The room that the appliances are associated with.
    roomArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSmartHomeAppliances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSmartHomeAppliances_maxResults' - The maximum number of appliances to be returned, per paginated calls.
--
-- 'nextToken', 'listSmartHomeAppliances_nextToken' - The tokens used for pagination.
--
-- 'roomArn', 'listSmartHomeAppliances_roomArn' - The room that the appliances are associated with.
newListSmartHomeAppliances ::
  -- | 'roomArn'
  Prelude.Text ->
  ListSmartHomeAppliances
newListSmartHomeAppliances pRoomArn_ =
  ListSmartHomeAppliances'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      roomArn = pRoomArn_
    }

-- | The maximum number of appliances to be returned, per paginated calls.
listSmartHomeAppliances_maxResults :: Lens.Lens' ListSmartHomeAppliances (Prelude.Maybe Prelude.Natural)
listSmartHomeAppliances_maxResults = Lens.lens (\ListSmartHomeAppliances' {maxResults} -> maxResults) (\s@ListSmartHomeAppliances' {} a -> s {maxResults = a} :: ListSmartHomeAppliances)

-- | The tokens used for pagination.
listSmartHomeAppliances_nextToken :: Lens.Lens' ListSmartHomeAppliances (Prelude.Maybe Prelude.Text)
listSmartHomeAppliances_nextToken = Lens.lens (\ListSmartHomeAppliances' {nextToken} -> nextToken) (\s@ListSmartHomeAppliances' {} a -> s {nextToken = a} :: ListSmartHomeAppliances)

-- | The room that the appliances are associated with.
listSmartHomeAppliances_roomArn :: Lens.Lens' ListSmartHomeAppliances Prelude.Text
listSmartHomeAppliances_roomArn = Lens.lens (\ListSmartHomeAppliances' {roomArn} -> roomArn) (\s@ListSmartHomeAppliances' {} a -> s {roomArn = a} :: ListSmartHomeAppliances)

instance Core.AWSPager ListSmartHomeAppliances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSmartHomeAppliancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSmartHomeAppliancesResponse_smartHomeAppliances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSmartHomeAppliances_nextToken
          Lens..~ rs
          Lens.^? listSmartHomeAppliancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSmartHomeAppliances where
  type
    AWSResponse ListSmartHomeAppliances =
      ListSmartHomeAppliancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSmartHomeAppliancesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SmartHomeAppliances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSmartHomeAppliances where
  hashWithSalt _salt ListSmartHomeAppliances' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` roomArn

instance Prelude.NFData ListSmartHomeAppliances where
  rnf ListSmartHomeAppliances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf roomArn

instance Data.ToHeaders ListSmartHomeAppliances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ListSmartHomeAppliances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSmartHomeAppliances where
  toJSON ListSmartHomeAppliances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("RoomArn" Data..= roomArn)
          ]
      )

instance Data.ToPath ListSmartHomeAppliances where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSmartHomeAppliances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSmartHomeAppliancesResponse' smart constructor.
data ListSmartHomeAppliancesResponse = ListSmartHomeAppliancesResponse'
  { -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The smart home appliances.
    smartHomeAppliances :: Prelude.Maybe [SmartHomeAppliance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSmartHomeAppliancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSmartHomeAppliancesResponse_nextToken' - The tokens used for pagination.
--
-- 'smartHomeAppliances', 'listSmartHomeAppliancesResponse_smartHomeAppliances' - The smart home appliances.
--
-- 'httpStatus', 'listSmartHomeAppliancesResponse_httpStatus' - The response's http status code.
newListSmartHomeAppliancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSmartHomeAppliancesResponse
newListSmartHomeAppliancesResponse pHttpStatus_ =
  ListSmartHomeAppliancesResponse'
    { nextToken =
        Prelude.Nothing,
      smartHomeAppliances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tokens used for pagination.
listSmartHomeAppliancesResponse_nextToken :: Lens.Lens' ListSmartHomeAppliancesResponse (Prelude.Maybe Prelude.Text)
listSmartHomeAppliancesResponse_nextToken = Lens.lens (\ListSmartHomeAppliancesResponse' {nextToken} -> nextToken) (\s@ListSmartHomeAppliancesResponse' {} a -> s {nextToken = a} :: ListSmartHomeAppliancesResponse)

-- | The smart home appliances.
listSmartHomeAppliancesResponse_smartHomeAppliances :: Lens.Lens' ListSmartHomeAppliancesResponse (Prelude.Maybe [SmartHomeAppliance])
listSmartHomeAppliancesResponse_smartHomeAppliances = Lens.lens (\ListSmartHomeAppliancesResponse' {smartHomeAppliances} -> smartHomeAppliances) (\s@ListSmartHomeAppliancesResponse' {} a -> s {smartHomeAppliances = a} :: ListSmartHomeAppliancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSmartHomeAppliancesResponse_httpStatus :: Lens.Lens' ListSmartHomeAppliancesResponse Prelude.Int
listSmartHomeAppliancesResponse_httpStatus = Lens.lens (\ListSmartHomeAppliancesResponse' {httpStatus} -> httpStatus) (\s@ListSmartHomeAppliancesResponse' {} a -> s {httpStatus = a} :: ListSmartHomeAppliancesResponse)

instance
  Prelude.NFData
    ListSmartHomeAppliancesResponse
  where
  rnf ListSmartHomeAppliancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf smartHomeAppliances
      `Prelude.seq` Prelude.rnf httpStatus
