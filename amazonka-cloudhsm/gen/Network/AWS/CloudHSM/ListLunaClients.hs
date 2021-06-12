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
-- Module      : Network.AWS.CloudHSM.ListLunaClients
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Lists all of the clients.
--
-- This operation supports pagination with the use of the @NextToken@
-- member. If more results are available, the @NextToken@ member of the
-- response contains a token that you pass in the next call to
-- @ListLunaClients@ to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListLunaClients
  ( -- * Creating a Request
    ListLunaClients (..),
    newListLunaClients,

    -- * Request Lenses
    listLunaClients_nextToken,

    -- * Destructuring the Response
    ListLunaClientsResponse (..),
    newListLunaClientsResponse,

    -- * Response Lenses
    listLunaClientsResponse_nextToken,
    listLunaClientsResponse_httpStatus,
    listLunaClientsResponse_clientList,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLunaClients' smart constructor.
data ListLunaClients = ListLunaClients'
  { -- | The @NextToken@ value from a previous call to @ListLunaClients@. Pass
    -- null if this is the first call.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLunaClients' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLunaClients_nextToken' - The @NextToken@ value from a previous call to @ListLunaClients@. Pass
-- null if this is the first call.
newListLunaClients ::
  ListLunaClients
newListLunaClients =
  ListLunaClients' {nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListLunaClients@. Pass
-- null if this is the first call.
listLunaClients_nextToken :: Lens.Lens' ListLunaClients (Core.Maybe Core.Text)
listLunaClients_nextToken = Lens.lens (\ListLunaClients' {nextToken} -> nextToken) (\s@ListLunaClients' {} a -> s {nextToken = a} :: ListLunaClients)

instance Core.AWSPager ListLunaClients where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLunaClientsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listLunaClientsResponse_clientList) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLunaClients_nextToken
          Lens..~ rs
          Lens.^? listLunaClientsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListLunaClients where
  type
    AWSResponse ListLunaClients =
      ListLunaClientsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLunaClientsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "ClientList" Core..!@ Core.mempty)
      )

instance Core.Hashable ListLunaClients

instance Core.NFData ListLunaClients

instance Core.ToHeaders ListLunaClients where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.ListLunaClients" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListLunaClients where
  toJSON ListLunaClients' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListLunaClients where
  toPath = Core.const "/"

instance Core.ToQuery ListLunaClients where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListLunaClientsResponse' smart constructor.
data ListLunaClientsResponse = ListLunaClientsResponse'
  { -- | If not null, more results are available. Pass this to @ListLunaClients@
    -- to retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The list of clients.
    clientList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLunaClientsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLunaClientsResponse_nextToken' - If not null, more results are available. Pass this to @ListLunaClients@
-- to retrieve the next set of items.
--
-- 'httpStatus', 'listLunaClientsResponse_httpStatus' - The response's http status code.
--
-- 'clientList', 'listLunaClientsResponse_clientList' - The list of clients.
newListLunaClientsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListLunaClientsResponse
newListLunaClientsResponse pHttpStatus_ =
  ListLunaClientsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      clientList = Core.mempty
    }

-- | If not null, more results are available. Pass this to @ListLunaClients@
-- to retrieve the next set of items.
listLunaClientsResponse_nextToken :: Lens.Lens' ListLunaClientsResponse (Core.Maybe Core.Text)
listLunaClientsResponse_nextToken = Lens.lens (\ListLunaClientsResponse' {nextToken} -> nextToken) (\s@ListLunaClientsResponse' {} a -> s {nextToken = a} :: ListLunaClientsResponse)

-- | The response's http status code.
listLunaClientsResponse_httpStatus :: Lens.Lens' ListLunaClientsResponse Core.Int
listLunaClientsResponse_httpStatus = Lens.lens (\ListLunaClientsResponse' {httpStatus} -> httpStatus) (\s@ListLunaClientsResponse' {} a -> s {httpStatus = a} :: ListLunaClientsResponse)

-- | The list of clients.
listLunaClientsResponse_clientList :: Lens.Lens' ListLunaClientsResponse [Core.Text]
listLunaClientsResponse_clientList = Lens.lens (\ListLunaClientsResponse' {clientList} -> clientList) (\s@ListLunaClientsResponse' {} a -> s {clientList = a} :: ListLunaClientsResponse) Core.. Lens._Coerce

instance Core.NFData ListLunaClientsResponse
