{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeListenerCertificates' smart constructor.
data DescribeListenerCertificates = DescribeListenerCertificates'
  { -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the listener.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeListenerCertificates
newDescribeListenerCertificates pListenerArn_ =
  DescribeListenerCertificates'
    { pageSize =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      listenerArn = pListenerArn_
    }

-- | The maximum number of results to return with this call.
describeListenerCertificates_pageSize :: Lens.Lens' DescribeListenerCertificates (Prelude.Maybe Prelude.Natural)
describeListenerCertificates_pageSize = Lens.lens (\DescribeListenerCertificates' {pageSize} -> pageSize) (\s@DescribeListenerCertificates' {} a -> s {pageSize = a} :: DescribeListenerCertificates)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeListenerCertificates_marker :: Lens.Lens' DescribeListenerCertificates (Prelude.Maybe Prelude.Text)
describeListenerCertificates_marker = Lens.lens (\DescribeListenerCertificates' {marker} -> marker) (\s@DescribeListenerCertificates' {} a -> s {marker = a} :: DescribeListenerCertificates)

-- | The Amazon Resource Names (ARN) of the listener.
describeListenerCertificates_listenerArn :: Lens.Lens' DescribeListenerCertificates Prelude.Text
describeListenerCertificates_listenerArn = Lens.lens (\DescribeListenerCertificates' {listenerArn} -> listenerArn) (\s@DescribeListenerCertificates' {} a -> s {listenerArn = a} :: DescribeListenerCertificates)

instance Pager.AWSPager DescribeListenerCertificates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeListenerCertificatesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeListenerCertificatesResponse_certificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeListenerCertificates_marker
          Lens..~ rs
          Lens.^? describeListenerCertificatesResponse_nextMarker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeListenerCertificates
  where
  type
    Rs DescribeListenerCertificates =
      DescribeListenerCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeListenerCertificatesResult"
      ( \s h x ->
          DescribeListenerCertificatesResponse'
            Prelude.<$> (x Prelude..@? "NextMarker")
            Prelude.<*> ( x Prelude..@? "Certificates"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeListenerCertificates

instance Prelude.NFData DescribeListenerCertificates

instance
  Prelude.ToHeaders
    DescribeListenerCertificates
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeListenerCertificates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeListenerCertificates where
  toQuery DescribeListenerCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeListenerCertificates" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "PageSize" Prelude.=: pageSize,
        "Marker" Prelude.=: marker,
        "ListenerArn" Prelude.=: listenerArn
      ]

-- | /See:/ 'newDescribeListenerCertificatesResponse' smart constructor.
data DescribeListenerCertificatesResponse = DescribeListenerCertificatesResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Information about the certificates.
    certificates :: Prelude.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeListenerCertificatesResponse
newDescribeListenerCertificatesResponse pHttpStatus_ =
  DescribeListenerCertificatesResponse'
    { nextMarker =
        Prelude.Nothing,
      certificates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeListenerCertificatesResponse_nextMarker :: Lens.Lens' DescribeListenerCertificatesResponse (Prelude.Maybe Prelude.Text)
describeListenerCertificatesResponse_nextMarker = Lens.lens (\DescribeListenerCertificatesResponse' {nextMarker} -> nextMarker) (\s@DescribeListenerCertificatesResponse' {} a -> s {nextMarker = a} :: DescribeListenerCertificatesResponse)

-- | Information about the certificates.
describeListenerCertificatesResponse_certificates :: Lens.Lens' DescribeListenerCertificatesResponse (Prelude.Maybe [Certificate])
describeListenerCertificatesResponse_certificates = Lens.lens (\DescribeListenerCertificatesResponse' {certificates} -> certificates) (\s@DescribeListenerCertificatesResponse' {} a -> s {certificates = a} :: DescribeListenerCertificatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeListenerCertificatesResponse_httpStatus :: Lens.Lens' DescribeListenerCertificatesResponse Prelude.Int
describeListenerCertificatesResponse_httpStatus = Lens.lens (\DescribeListenerCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeListenerCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeListenerCertificatesResponse)

instance
  Prelude.NFData
    DescribeListenerCertificatesResponse
