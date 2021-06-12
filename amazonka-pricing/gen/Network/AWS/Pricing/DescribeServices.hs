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
-- Module      : Network.AWS.Pricing.DescribeServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata for one service or a list of the metadata for all
-- services. Use this without a service code to get the service codes for
-- all services. Use it with a service code, such as @AmazonEC2@, to get
-- information specific to that service, such as the attribute names
-- available for that service. For example, some of the attribute names
-- available for EC2 are @volumeType@, @maxIopsVolume@, @operation@,
-- @locationType@, and @instanceCapacity10xlarge@.
--
-- This operation returns paginated results.
module Network.AWS.Pricing.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_nextToken,
    describeServices_maxResults,
    describeServices_serviceCode,
    describeServices_formatVersion,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_nextToken,
    describeServicesResponse_services,
    describeServicesResponse_formatVersion,
    describeServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pricing.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that you want returned in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The code for the service whose information you want to retrieve, such as
    -- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
    -- @GetProducts@ call. To retrieve a list of all services, leave this
    -- blank.
    serviceCode :: Core.Maybe Core.Text,
    -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServices_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'maxResults', 'describeServices_maxResults' - The maximum number of results that you want returned in the response.
--
-- 'serviceCode', 'describeServices_serviceCode' - The code for the service whose information you want to retrieve, such as
-- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
-- @GetProducts@ call. To retrieve a list of all services, leave this
-- blank.
--
-- 'formatVersion', 'describeServices_formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      serviceCode = Core.Nothing,
      formatVersion = Core.Nothing
    }

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
describeServices_nextToken :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
describeServices_nextToken = Lens.lens (\DescribeServices' {nextToken} -> nextToken) (\s@DescribeServices' {} a -> s {nextToken = a} :: DescribeServices)

-- | The maximum number of results that you want returned in the response.
describeServices_maxResults :: Lens.Lens' DescribeServices (Core.Maybe Core.Natural)
describeServices_maxResults = Lens.lens (\DescribeServices' {maxResults} -> maxResults) (\s@DescribeServices' {} a -> s {maxResults = a} :: DescribeServices)

-- | The code for the service whose information you want to retrieve, such as
-- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
-- @GetProducts@ call. To retrieve a list of all services, leave this
-- blank.
describeServices_serviceCode :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
describeServices_serviceCode = Lens.lens (\DescribeServices' {serviceCode} -> serviceCode) (\s@DescribeServices' {} a -> s {serviceCode = a} :: DescribeServices)

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
describeServices_formatVersion :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
describeServices_formatVersion = Lens.lens (\DescribeServices' {formatVersion} -> formatVersion) (\s@DescribeServices' {} a -> s {formatVersion = a} :: DescribeServices)

instance Core.AWSPager DescribeServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeServicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeServicesResponse_services Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeServices_nextToken
          Lens..~ rs
          Lens.^? describeServicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Services" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "FormatVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeServices

instance Core.NFData DescribeServices

instance Core.ToHeaders DescribeServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPriceListService.DescribeServices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ServiceCode" Core..=) Core.<$> serviceCode,
            ("FormatVersion" Core..=) Core.<$> formatVersion
          ]
      )

instance Core.ToPath DescribeServices where
  toPath = Core.const "/"

instance Core.ToQuery DescribeServices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | The pagination token for the next set of retreivable results.
    nextToken :: Core.Maybe Core.Text,
    -- | The service metadata for the service or services in the response.
    services :: Core.Maybe [PricingService],
    -- | The format version of the response. For example, @aws_v1@.
    formatVersion :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServicesResponse_nextToken' - The pagination token for the next set of retreivable results.
--
-- 'services', 'describeServicesResponse_services' - The service metadata for the service or services in the response.
--
-- 'formatVersion', 'describeServicesResponse_formatVersion' - The format version of the response. For example, @aws_v1@.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { nextToken = Core.Nothing,
      services = Core.Nothing,
      formatVersion = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token for the next set of retreivable results.
describeServicesResponse_nextToken :: Lens.Lens' DescribeServicesResponse (Core.Maybe Core.Text)
describeServicesResponse_nextToken = Lens.lens (\DescribeServicesResponse' {nextToken} -> nextToken) (\s@DescribeServicesResponse' {} a -> s {nextToken = a} :: DescribeServicesResponse)

-- | The service metadata for the service or services in the response.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Core.Maybe [PricingService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The format version of the response. For example, @aws_v1@.
describeServicesResponse_formatVersion :: Lens.Lens' DescribeServicesResponse (Core.Maybe Core.Text)
describeServicesResponse_formatVersion = Lens.lens (\DescribeServicesResponse' {formatVersion} -> formatVersion) (\s@DescribeServicesResponse' {} a -> s {formatVersion = a} :: DescribeServicesResponse)

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Core.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Core.NFData DescribeServicesResponse
