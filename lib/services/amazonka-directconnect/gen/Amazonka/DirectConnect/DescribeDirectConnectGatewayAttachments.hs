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
-- Module      : Amazonka.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attachments between your Direct Connect gateways and virtual
-- interfaces. You must specify a Direct Connect gateway, a virtual
-- interface, or both. If you specify a Direct Connect gateway, the
-- response contains all virtual interfaces attached to the Direct Connect
-- gateway. If you specify a virtual interface, the response contains all
-- Direct Connect gateways attached to the virtual interface. If you
-- specify both, the response contains the attachment between the Direct
-- Connect gateway and the virtual interface.
--
-- This operation returns paginated results.
module Amazonka.DirectConnect.DescribeDirectConnectGatewayAttachments
  ( -- * Creating a Request
    DescribeDirectConnectGatewayAttachments (..),
    newDescribeDirectConnectGatewayAttachments,

    -- * Request Lenses
    describeDirectConnectGatewayAttachments_directConnectGatewayId,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,

    -- * Destructuring the Response
    DescribeDirectConnectGatewayAttachmentsResponse (..),
    newDescribeDirectConnectGatewayAttachmentsResponse,

    -- * Response Lenses
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayId', 'describeDirectConnectGatewayAttachments_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'maxResults', 'describeDirectConnectGatewayAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'nextToken', 'describeDirectConnectGatewayAttachments_nextToken' - The token provided in the previous call to retrieve the next page.
--
-- 'virtualInterfaceId', 'describeDirectConnectGatewayAttachments_virtualInterfaceId' - The ID of the virtual interface.
newDescribeDirectConnectGatewayAttachments ::
  DescribeDirectConnectGatewayAttachments
newDescribeDirectConnectGatewayAttachments =
  DescribeDirectConnectGatewayAttachments'
    { directConnectGatewayId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      virtualInterfaceId =
        Prelude.Nothing
    }

-- | The ID of the Direct Connect gateway.
describeDirectConnectGatewayAttachments_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGatewayAttachments' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAttachments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGatewayAttachments_maxResults :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Int)
describeDirectConnectGatewayAttachments_maxResults = Lens.lens (\DescribeDirectConnectGatewayAttachments' {maxResults} -> maxResults) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {maxResults = a} :: DescribeDirectConnectGatewayAttachments)

-- | The token provided in the previous call to retrieve the next page.
describeDirectConnectGatewayAttachments_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_nextToken = Lens.lens (\DescribeDirectConnectGatewayAttachments' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachments)

-- | The ID of the virtual interface.
describeDirectConnectGatewayAttachments_virtualInterfaceId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_virtualInterfaceId = Lens.lens (\DescribeDirectConnectGatewayAttachments' {virtualInterfaceId} -> virtualInterfaceId) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {virtualInterfaceId = a} :: DescribeDirectConnectGatewayAttachments)

instance
  Core.AWSPager
    DescribeDirectConnectGatewayAttachments
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAttachmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDirectConnectGatewayAttachments_nextToken
          Lens..~ rs
            Lens.^? describeDirectConnectGatewayAttachmentsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDirectConnectGatewayAttachments
  where
  type
    AWSResponse
      DescribeDirectConnectGatewayAttachments =
      DescribeDirectConnectGatewayAttachmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAttachmentsResponse'
            Prelude.<$> ( x Data..?> "directConnectGatewayAttachments"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDirectConnectGatewayAttachments
  where
  hashWithSalt
    _salt
    DescribeDirectConnectGatewayAttachments' {..} =
      _salt `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` virtualInterfaceId

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAttachments
  where
  rnf DescribeDirectConnectGatewayAttachments' {..} =
    Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf virtualInterfaceId

instance
  Data.ToHeaders
    DescribeDirectConnectGatewayAttachments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeDirectConnectGatewayAttachments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeDirectConnectGatewayAttachments
  where
  toJSON DescribeDirectConnectGatewayAttachments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("directConnectGatewayId" Data..=)
              Prelude.<$> directConnectGatewayId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("virtualInterfaceId" Data..=)
              Prelude.<$> virtualInterfaceId
          ]
      )

instance
  Data.ToPath
    DescribeDirectConnectGatewayAttachments
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDirectConnectGatewayAttachments
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { -- | The attachments.
    directConnectGatewayAttachments :: Prelude.Maybe [DirectConnectGatewayAttachment],
    -- | The token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAttachments', 'describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments' - The attachments.
--
-- 'nextToken', 'describeDirectConnectGatewayAttachmentsResponse_nextToken' - The token to retrieve the next page.
--
-- 'httpStatus', 'describeDirectConnectGatewayAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewayAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectConnectGatewayAttachmentsResponse
newDescribeDirectConnectGatewayAttachmentsResponse
  pHttpStatus_ =
    DescribeDirectConnectGatewayAttachmentsResponse'
      { directConnectGatewayAttachments =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The attachments.
describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Prelude.Maybe [DirectConnectGatewayAttachment])
describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {directConnectGatewayAttachments} -> directConnectGatewayAttachments) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {directConnectGatewayAttachments = a} :: DescribeDirectConnectGatewayAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next page.
describeDirectConnectGatewayAttachmentsResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachmentsResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachmentsResponse)

-- | The response's http status code.
describeDirectConnectGatewayAttachmentsResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse Prelude.Int
describeDirectConnectGatewayAttachmentsResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewayAttachmentsResponse)

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAttachmentsResponse
  where
  rnf
    DescribeDirectConnectGatewayAttachmentsResponse' {..} =
      Prelude.rnf directConnectGatewayAttachments
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
