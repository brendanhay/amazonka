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
-- Module      : Amazonka.RAM.GetResourceShareInvitations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the invitations that you have received for resource shares.
--
-- This operation returns paginated results.
module Amazonka.RAM.GetResourceShareInvitations
  ( -- * Creating a Request
    GetResourceShareInvitations (..),
    newGetResourceShareInvitations,

    -- * Request Lenses
    getResourceShareInvitations_nextToken,
    getResourceShareInvitations_resourceShareInvitationArns,
    getResourceShareInvitations_maxResults,
    getResourceShareInvitations_resourceShareArns,

    -- * Destructuring the Response
    GetResourceShareInvitationsResponse (..),
    newGetResourceShareInvitationsResponse,

    -- * Response Lenses
    getResourceShareInvitationsResponse_resourceShareInvitations,
    getResourceShareInvitationsResponse_nextToken,
    getResourceShareInvitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceShareInvitations' smart constructor.
data GetResourceShareInvitations = GetResourceShareInvitations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the invitations.
    resourceShareInvitationArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the resource shares.
    resourceShareArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShareInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceShareInvitations_nextToken' - The token for the next page of results.
--
-- 'resourceShareInvitationArns', 'getResourceShareInvitations_resourceShareInvitationArns' - The Amazon Resource Names (ARN) of the invitations.
--
-- 'maxResults', 'getResourceShareInvitations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareArns', 'getResourceShareInvitations_resourceShareArns' - The Amazon Resource Names (ARN) of the resource shares.
newGetResourceShareInvitations ::
  GetResourceShareInvitations
newGetResourceShareInvitations =
  GetResourceShareInvitations'
    { nextToken =
        Prelude.Nothing,
      resourceShareInvitationArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing
    }

-- | The token for the next page of results.
getResourceShareInvitations_nextToken :: Lens.Lens' GetResourceShareInvitations (Prelude.Maybe Prelude.Text)
getResourceShareInvitations_nextToken = Lens.lens (\GetResourceShareInvitations' {nextToken} -> nextToken) (\s@GetResourceShareInvitations' {} a -> s {nextToken = a} :: GetResourceShareInvitations)

-- | The Amazon Resource Names (ARN) of the invitations.
getResourceShareInvitations_resourceShareInvitationArns :: Lens.Lens' GetResourceShareInvitations (Prelude.Maybe [Prelude.Text])
getResourceShareInvitations_resourceShareInvitationArns = Lens.lens (\GetResourceShareInvitations' {resourceShareInvitationArns} -> resourceShareInvitationArns) (\s@GetResourceShareInvitations' {} a -> s {resourceShareInvitationArns = a} :: GetResourceShareInvitations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getResourceShareInvitations_maxResults :: Lens.Lens' GetResourceShareInvitations (Prelude.Maybe Prelude.Natural)
getResourceShareInvitations_maxResults = Lens.lens (\GetResourceShareInvitations' {maxResults} -> maxResults) (\s@GetResourceShareInvitations' {} a -> s {maxResults = a} :: GetResourceShareInvitations)

-- | The Amazon Resource Names (ARN) of the resource shares.
getResourceShareInvitations_resourceShareArns :: Lens.Lens' GetResourceShareInvitations (Prelude.Maybe [Prelude.Text])
getResourceShareInvitations_resourceShareArns = Lens.lens (\GetResourceShareInvitations' {resourceShareArns} -> resourceShareArns) (\s@GetResourceShareInvitations' {} a -> s {resourceShareArns = a} :: GetResourceShareInvitations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager GetResourceShareInvitations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceShareInvitationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceShareInvitationsResponse_resourceShareInvitations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourceShareInvitations_nextToken
          Lens..~ rs
          Lens.^? getResourceShareInvitationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourceShareInvitations where
  type
    AWSResponse GetResourceShareInvitations =
      GetResourceShareInvitationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceShareInvitationsResponse'
            Prelude.<$> ( x Core..?> "resourceShareInvitations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceShareInvitations where
  hashWithSalt salt' GetResourceShareInvitations' {..} =
    salt' `Prelude.hashWithSalt` resourceShareArns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceShareInvitationArns
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetResourceShareInvitations where
  rnf GetResourceShareInvitations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShareArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceShareInvitationArns

instance Core.ToHeaders GetResourceShareInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourceShareInvitations where
  toJSON GetResourceShareInvitations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("resourceShareInvitationArns" Core..=)
              Prelude.<$> resourceShareInvitationArns,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("resourceShareArns" Core..=)
              Prelude.<$> resourceShareArns
          ]
      )

instance Core.ToPath GetResourceShareInvitations where
  toPath = Prelude.const "/getresourceshareinvitations"

instance Core.ToQuery GetResourceShareInvitations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceShareInvitationsResponse' smart constructor.
data GetResourceShareInvitationsResponse = GetResourceShareInvitationsResponse'
  { -- | Information about the invitations.
    resourceShareInvitations :: Prelude.Maybe [ResourceShareInvitation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShareInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceShareInvitations', 'getResourceShareInvitationsResponse_resourceShareInvitations' - Information about the invitations.
--
-- 'nextToken', 'getResourceShareInvitationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getResourceShareInvitationsResponse_httpStatus' - The response's http status code.
newGetResourceShareInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceShareInvitationsResponse
newGetResourceShareInvitationsResponse pHttpStatus_ =
  GetResourceShareInvitationsResponse'
    { resourceShareInvitations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the invitations.
getResourceShareInvitationsResponse_resourceShareInvitations :: Lens.Lens' GetResourceShareInvitationsResponse (Prelude.Maybe [ResourceShareInvitation])
getResourceShareInvitationsResponse_resourceShareInvitations = Lens.lens (\GetResourceShareInvitationsResponse' {resourceShareInvitations} -> resourceShareInvitations) (\s@GetResourceShareInvitationsResponse' {} a -> s {resourceShareInvitations = a} :: GetResourceShareInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getResourceShareInvitationsResponse_nextToken :: Lens.Lens' GetResourceShareInvitationsResponse (Prelude.Maybe Prelude.Text)
getResourceShareInvitationsResponse_nextToken = Lens.lens (\GetResourceShareInvitationsResponse' {nextToken} -> nextToken) (\s@GetResourceShareInvitationsResponse' {} a -> s {nextToken = a} :: GetResourceShareInvitationsResponse)

-- | The response's http status code.
getResourceShareInvitationsResponse_httpStatus :: Lens.Lens' GetResourceShareInvitationsResponse Prelude.Int
getResourceShareInvitationsResponse_httpStatus = Lens.lens (\GetResourceShareInvitationsResponse' {httpStatus} -> httpStatus) (\s@GetResourceShareInvitationsResponse' {} a -> s {httpStatus = a} :: GetResourceShareInvitationsResponse)

instance
  Prelude.NFData
    GetResourceShareInvitationsResponse
  where
  rnf GetResourceShareInvitationsResponse' {..} =
    Prelude.rnf resourceShareInvitations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
