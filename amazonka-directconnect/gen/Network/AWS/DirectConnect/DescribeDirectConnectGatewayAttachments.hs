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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
  ( -- * Creating a Request
    DescribeDirectConnectGatewayAttachments (..),
    newDescribeDirectConnectGatewayAttachments,

    -- * Request Lenses
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,
    describeDirectConnectGatewayAttachments_directConnectGatewayId,

    -- * Destructuring the Response
    DescribeDirectConnectGatewayAttachmentsResponse (..),
    newDescribeDirectConnectGatewayAttachmentsResponse,

    -- * Response Lenses
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'describeDirectConnectGatewayAttachments_nextToken' - The token provided in the previous call to retrieve the next page.
--
-- 'maxResults', 'describeDirectConnectGatewayAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'virtualInterfaceId', 'describeDirectConnectGatewayAttachments_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'directConnectGatewayId', 'describeDirectConnectGatewayAttachments_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDescribeDirectConnectGatewayAttachments ::
  DescribeDirectConnectGatewayAttachments
newDescribeDirectConnectGatewayAttachments =
  DescribeDirectConnectGatewayAttachments'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      virtualInterfaceId =
        Prelude.Nothing,
      directConnectGatewayId =
        Prelude.Nothing
    }

-- | The token provided in the previous call to retrieve the next page.
describeDirectConnectGatewayAttachments_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_nextToken = Lens.lens (\DescribeDirectConnectGatewayAttachments' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGatewayAttachments_maxResults :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Int)
describeDirectConnectGatewayAttachments_maxResults = Lens.lens (\DescribeDirectConnectGatewayAttachments' {maxResults} -> maxResults) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {maxResults = a} :: DescribeDirectConnectGatewayAttachments)

-- | The ID of the virtual interface.
describeDirectConnectGatewayAttachments_virtualInterfaceId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_virtualInterfaceId = Lens.lens (\DescribeDirectConnectGatewayAttachments' {virtualInterfaceId} -> virtualInterfaceId) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {virtualInterfaceId = a} :: DescribeDirectConnectGatewayAttachments)

-- | The ID of the Direct Connect gateway.
describeDirectConnectGatewayAttachments_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachments_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGatewayAttachments' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGatewayAttachments' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAttachments)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAttachmentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> ( x Core..?> "directConnectGatewayAttachments"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDirectConnectGatewayAttachments

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAttachments

instance
  Core.ToHeaders
    DescribeDirectConnectGatewayAttachments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeDirectConnectGatewayAttachments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeDirectConnectGatewayAttachments
  where
  toJSON DescribeDirectConnectGatewayAttachments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("virtualInterfaceId" Core..=)
              Prelude.<$> virtualInterfaceId,
            ("directConnectGatewayId" Core..=)
              Prelude.<$> directConnectGatewayId
          ]
      )

instance
  Core.ToPath
    DescribeDirectConnectGatewayAttachments
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDirectConnectGatewayAttachments
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { -- | The token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The attachments.
    directConnectGatewayAttachments :: Prelude.Maybe [DirectConnectGatewayAttachment],
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
-- 'nextToken', 'describeDirectConnectGatewayAttachmentsResponse_nextToken' - The token to retrieve the next page.
--
-- 'directConnectGatewayAttachments', 'describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments' - The attachments.
--
-- 'httpStatus', 'describeDirectConnectGatewayAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewayAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectConnectGatewayAttachmentsResponse
newDescribeDirectConnectGatewayAttachmentsResponse
  pHttpStatus_ =
    DescribeDirectConnectGatewayAttachmentsResponse'
      { nextToken =
          Prelude.Nothing,
        directConnectGatewayAttachments =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to retrieve the next page.
describeDirectConnectGatewayAttachmentsResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAttachmentsResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachmentsResponse)

-- | The attachments.
describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Prelude.Maybe [DirectConnectGatewayAttachment])
describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {directConnectGatewayAttachments} -> directConnectGatewayAttachments) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {directConnectGatewayAttachments = a} :: DescribeDirectConnectGatewayAttachmentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDirectConnectGatewayAttachmentsResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse Prelude.Int
describeDirectConnectGatewayAttachmentsResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewayAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewayAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewayAttachmentsResponse)

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAttachmentsResponse
