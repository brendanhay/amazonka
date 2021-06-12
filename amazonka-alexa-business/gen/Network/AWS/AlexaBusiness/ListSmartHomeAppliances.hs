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
-- Module      : Network.AWS.AlexaBusiness.ListSmartHomeAppliances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the smart home appliances associated with a room.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSmartHomeAppliances
  ( -- * Creating a Request
    ListSmartHomeAppliances (..),
    newListSmartHomeAppliances,

    -- * Request Lenses
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_maxResults,
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSmartHomeAppliances' smart constructor.
data ListSmartHomeAppliances = ListSmartHomeAppliances'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of appliances to be returned, per paginated calls.
    maxResults :: Core.Maybe Core.Natural,
    -- | The room that the appliances are associated with.
    roomArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSmartHomeAppliances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSmartHomeAppliances_nextToken' - The tokens used for pagination.
--
-- 'maxResults', 'listSmartHomeAppliances_maxResults' - The maximum number of appliances to be returned, per paginated calls.
--
-- 'roomArn', 'listSmartHomeAppliances_roomArn' - The room that the appliances are associated with.
newListSmartHomeAppliances ::
  -- | 'roomArn'
  Core.Text ->
  ListSmartHomeAppliances
newListSmartHomeAppliances pRoomArn_ =
  ListSmartHomeAppliances'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      roomArn = pRoomArn_
    }

-- | The tokens used for pagination.
listSmartHomeAppliances_nextToken :: Lens.Lens' ListSmartHomeAppliances (Core.Maybe Core.Text)
listSmartHomeAppliances_nextToken = Lens.lens (\ListSmartHomeAppliances' {nextToken} -> nextToken) (\s@ListSmartHomeAppliances' {} a -> s {nextToken = a} :: ListSmartHomeAppliances)

-- | The maximum number of appliances to be returned, per paginated calls.
listSmartHomeAppliances_maxResults :: Lens.Lens' ListSmartHomeAppliances (Core.Maybe Core.Natural)
listSmartHomeAppliances_maxResults = Lens.lens (\ListSmartHomeAppliances' {maxResults} -> maxResults) (\s@ListSmartHomeAppliances' {} a -> s {maxResults = a} :: ListSmartHomeAppliances)

-- | The room that the appliances are associated with.
listSmartHomeAppliances_roomArn :: Lens.Lens' ListSmartHomeAppliances Core.Text
listSmartHomeAppliances_roomArn = Lens.lens (\ListSmartHomeAppliances' {roomArn} -> roomArn) (\s@ListSmartHomeAppliances' {} a -> s {roomArn = a} :: ListSmartHomeAppliances)

instance Core.AWSPager ListSmartHomeAppliances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSmartHomeAppliancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSmartHomeAppliancesResponse_smartHomeAppliances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSmartHomeAppliances_nextToken
          Lens..~ rs
          Lens.^? listSmartHomeAppliancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSmartHomeAppliances where
  type
    AWSResponse ListSmartHomeAppliances =
      ListSmartHomeAppliancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSmartHomeAppliancesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "SmartHomeAppliances"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSmartHomeAppliances

instance Core.NFData ListSmartHomeAppliances

instance Core.ToHeaders ListSmartHomeAppliances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListSmartHomeAppliances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSmartHomeAppliances where
  toJSON ListSmartHomeAppliances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("RoomArn" Core..= roomArn)
          ]
      )

instance Core.ToPath ListSmartHomeAppliances where
  toPath = Core.const "/"

instance Core.ToQuery ListSmartHomeAppliances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSmartHomeAppliancesResponse' smart constructor.
data ListSmartHomeAppliancesResponse = ListSmartHomeAppliancesResponse'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The smart home appliances.
    smartHomeAppliances :: Core.Maybe [SmartHomeAppliance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSmartHomeAppliancesResponse
newListSmartHomeAppliancesResponse pHttpStatus_ =
  ListSmartHomeAppliancesResponse'
    { nextToken =
        Core.Nothing,
      smartHomeAppliances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tokens used for pagination.
listSmartHomeAppliancesResponse_nextToken :: Lens.Lens' ListSmartHomeAppliancesResponse (Core.Maybe Core.Text)
listSmartHomeAppliancesResponse_nextToken = Lens.lens (\ListSmartHomeAppliancesResponse' {nextToken} -> nextToken) (\s@ListSmartHomeAppliancesResponse' {} a -> s {nextToken = a} :: ListSmartHomeAppliancesResponse)

-- | The smart home appliances.
listSmartHomeAppliancesResponse_smartHomeAppliances :: Lens.Lens' ListSmartHomeAppliancesResponse (Core.Maybe [SmartHomeAppliance])
listSmartHomeAppliancesResponse_smartHomeAppliances = Lens.lens (\ListSmartHomeAppliancesResponse' {smartHomeAppliances} -> smartHomeAppliances) (\s@ListSmartHomeAppliancesResponse' {} a -> s {smartHomeAppliances = a} :: ListSmartHomeAppliancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSmartHomeAppliancesResponse_httpStatus :: Lens.Lens' ListSmartHomeAppliancesResponse Core.Int
listSmartHomeAppliancesResponse_httpStatus = Lens.lens (\ListSmartHomeAppliancesResponse' {httpStatus} -> httpStatus) (\s@ListSmartHomeAppliancesResponse' {} a -> s {httpStatus = a} :: ListSmartHomeAppliancesResponse)

instance Core.NFData ListSmartHomeAppliancesResponse
