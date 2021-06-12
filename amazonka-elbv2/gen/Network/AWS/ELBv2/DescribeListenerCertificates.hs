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
-- Module      : Network.AWS.ELBv2.DescribeListenerCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default certificate and the certificate list for the
-- specified HTTPS or TLS listener.
--
-- If the default certificate is also in the certificate list, it appears
-- twice in the results (once with @IsDefault@ set to true and once with
-- @IsDefault@ set to false).
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#https-listener-certificates SSL certificates>
-- in the /Application Load Balancers Guide/ or
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#tls-listener-certificate Server certificates>
-- in the /Network Load Balancers Guide/.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeListenerCertificates
  ( -- * Creating a Request
    DescribeListenerCertificates (..),
    newDescribeListenerCertificates,

    -- * Request Lenses
    describeListenerCertificates_pageSize,
    describeListenerCertificates_marker,
    describeListenerCertificates_listenerArn,

    -- * Destructuring the Response
    DescribeListenerCertificatesResponse (..),
    newDescribeListenerCertificatesResponse,

    -- * Response Lenses
    describeListenerCertificatesResponse_nextMarker,
    describeListenerCertificatesResponse_certificates,
    describeListenerCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text,
    -- | The Amazon Resource Names (ARN) of the listener.
    listenerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeListenerCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describeListenerCertificates_pageSize' - The maximum number of results to return with this call.
--
-- 'marker', 'describeListenerCertificates_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'listenerArn', 'describeListenerCertificates_listenerArn' - The Amazon Resource Names (ARN) of the listener.
newDescribeListenerCertificates ::
  -- | 'listenerArn'
  Core.Text ->
  DescribeListenerCertificates
newDescribeListenerCertificates pListenerArn_ =
  DescribeListenerCertificates'
    { pageSize =
        Core.Nothing,
      marker = Core.Nothing,
      listenerArn = pListenerArn_
    }

-- | The maximum number of results to return with this call.
describeListenerCertificates_pageSize :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Core.Natural)
describeListenerCertificates_pageSize = Lens.lens (\DescribeListenerCertificates' {pageSize} -> pageSize) (\s@DescribeListenerCertificates' {} a -> s {pageSize = a} :: DescribeListenerCertificates)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeListenerCertificates_marker :: Lens.Lens' DescribeListenerCertificates (Core.Maybe Core.Text)
describeListenerCertificates_marker = Lens.lens (\DescribeListenerCertificates' {marker} -> marker) (\s@DescribeListenerCertificates' {} a -> s {marker = a} :: DescribeListenerCertificates)

-- | The Amazon Resource Names (ARN) of the listener.
describeListenerCertificates_listenerArn :: Lens.Lens' DescribeListenerCertificates Core.Text
describeListenerCertificates_listenerArn = Lens.lens (\DescribeListenerCertificates' {listenerArn} -> listenerArn) (\s@DescribeListenerCertificates' {} a -> s {listenerArn = a} :: DescribeListenerCertificates)

instance Core.AWSPager DescribeListenerCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeListenerCertificatesResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeListenerCertificatesResponse_certificates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeListenerCertificates_marker
          Lens..~ rs
          Lens.^? describeListenerCertificatesResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest DescribeListenerCertificates where
  type
    AWSResponse DescribeListenerCertificates =
      DescribeListenerCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeListenerCertificatesResult"
      ( \s h x ->
          DescribeListenerCertificatesResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> ( x Core..@? "Certificates" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeListenerCertificates

instance Core.NFData DescribeListenerCertificates

instance Core.ToHeaders DescribeListenerCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeListenerCertificates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeListenerCertificates where
  toQuery DescribeListenerCertificates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeListenerCertificates" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "PageSize" Core.=: pageSize,
        "Marker" Core.=: marker,
        "ListenerArn" Core.=: listenerArn
      ]

-- | /See:/ 'newDescribeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Core.Maybe Core.Text,
    -- | Information about the certificates.
    certificates :: Core.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeListenerCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeListenerCertificatesResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'certificates', 'describeListenerCertificatesResponse_certificates' - Information about the certificates.
--
-- 'httpStatus', 'describeListenerCertificatesResponse_httpStatus' - The response's http status code.
newDescribeListenerCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeListenerCertificatesResponse
newDescribeListenerCertificatesResponse pHttpStatus_ =
  DescribeListenerCertificatesResponse'
    { nextMarker =
        Core.Nothing,
      certificates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeListenerCertificatesResponse_nextMarker :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe Core.Text)
describeListenerCertificatesResponse_nextMarker = Lens.lens (\DescribeListenerCertificatesResponse' {nextMarker} -> nextMarker) (\s@DescribeListenerCertificatesResponse' {} a -> s {nextMarker = a} :: DescribeListenerCertificatesResponse)

-- | Information about the certificates.
describeListenerCertificatesResponse_certificates :: Lens.Lens' DescribeListenerCertificatesResponse (Core.Maybe [Certificate])
describeListenerCertificatesResponse_certificates = Lens.lens (\DescribeListenerCertificatesResponse' {certificates} -> certificates) (\s@DescribeListenerCertificatesResponse' {} a -> s {certificates = a} :: DescribeListenerCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeListenerCertificatesResponse_httpStatus :: Lens.Lens' DescribeListenerCertificatesResponse Core.Int
describeListenerCertificatesResponse_httpStatus = Lens.lens (\DescribeListenerCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeListenerCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeListenerCertificatesResponse)

instance
  Core.NFData
    DescribeListenerCertificatesResponse
