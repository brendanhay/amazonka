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
-- Module      : Amazonka.Pricing.DescribeServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Pricing.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_formatVersion,
    describeServices_maxResults,
    describeServices_nextToken,
    describeServices_serviceCode,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_formatVersion,
    describeServicesResponse_nextToken,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Pricing.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that you want returned in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The code for the service whose information you want to retrieve, such as
    -- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
    -- @GetProducts@ call. To retrieve a list of all services, leave this
    -- blank.
    serviceCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatVersion', 'describeServices_formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- 'maxResults', 'describeServices_maxResults' - The maximum number of results that you want returned in the response.
--
-- 'nextToken', 'describeServices_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'serviceCode', 'describeServices_serviceCode' - The code for the service whose information you want to retrieve, such as
-- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
-- @GetProducts@ call. To retrieve a list of all services, leave this
-- blank.
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { formatVersion = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceCode = Prelude.Nothing
    }

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
describeServices_formatVersion :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Text)
describeServices_formatVersion = Lens.lens (\DescribeServices' {formatVersion} -> formatVersion) (\s@DescribeServices' {} a -> s {formatVersion = a} :: DescribeServices)

-- | The maximum number of results that you want returned in the response.
describeServices_maxResults :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Natural)
describeServices_maxResults = Lens.lens (\DescribeServices' {maxResults} -> maxResults) (\s@DescribeServices' {} a -> s {maxResults = a} :: DescribeServices)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
describeServices_nextToken :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Text)
describeServices_nextToken = Lens.lens (\DescribeServices' {nextToken} -> nextToken) (\s@DescribeServices' {} a -> s {nextToken = a} :: DescribeServices)

-- | The code for the service whose information you want to retrieve, such as
-- @AmazonEC2@. You can use the @ServiceCode@ to filter the results in a
-- @GetProducts@ call. To retrieve a list of all services, leave this
-- blank.
describeServices_serviceCode :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Text)
describeServices_serviceCode = Lens.lens (\DescribeServices' {serviceCode} -> serviceCode) (\s@DescribeServices' {} a -> s {serviceCode = a} :: DescribeServices)

instance Core.AWSPager DescribeServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeServicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeServicesResponse_services
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeServices_nextToken
          Lens..~ rs
          Lens.^? describeServicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Prelude.<$> (x Data..?> "FormatVersion")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Services" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServices where
  hashWithSalt _salt DescribeServices' {..} =
    _salt
      `Prelude.hashWithSalt` formatVersion
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData DescribeServices where
  rnf DescribeServices' {..} =
    Prelude.rnf formatVersion
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceCode

instance Data.ToHeaders DescribeServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPriceListService.DescribeServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatVersion" Data..=) Prelude.<$> formatVersion,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ServiceCode" Data..=) Prelude.<$> serviceCode
          ]
      )

instance Data.ToPath DescribeServices where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | The format version of the response. For example, @aws_v1@.
    formatVersion :: Prelude.Maybe Prelude.Text,
    -- | The pagination token for the next set of retrievable results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The service metadata for the service or services in the response.
    services :: Prelude.Maybe [PricingService],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatVersion', 'describeServicesResponse_formatVersion' - The format version of the response. For example, @aws_v1@.
--
-- 'nextToken', 'describeServicesResponse_nextToken' - The pagination token for the next set of retrievable results.
--
-- 'services', 'describeServicesResponse_services' - The service metadata for the service or services in the response.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { formatVersion =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      services = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The format version of the response. For example, @aws_v1@.
describeServicesResponse_formatVersion :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe Prelude.Text)
describeServicesResponse_formatVersion = Lens.lens (\DescribeServicesResponse' {formatVersion} -> formatVersion) (\s@DescribeServicesResponse' {} a -> s {formatVersion = a} :: DescribeServicesResponse)

-- | The pagination token for the next set of retrievable results.
describeServicesResponse_nextToken :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe Prelude.Text)
describeServicesResponse_nextToken = Lens.lens (\DescribeServicesResponse' {nextToken} -> nextToken) (\s@DescribeServicesResponse' {} a -> s {nextToken = a} :: DescribeServicesResponse)

-- | The service metadata for the service or services in the response.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe [PricingService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Prelude.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Prelude.NFData DescribeServicesResponse where
  rnf DescribeServicesResponse' {..} =
    Prelude.rnf formatVersion
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf httpStatus
