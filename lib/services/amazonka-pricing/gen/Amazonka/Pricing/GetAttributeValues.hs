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
-- Module      : Amazonka.Pricing.GetAttributeValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attribute values. Attributes are similar to the
-- details in a Price List API offer file. For a list of available
-- attributes, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs Offer File Definitions>
-- in the
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html Billing and Cost Management User Guide>.
--
-- This operation returns paginated results.
module Amazonka.Pricing.GetAttributeValues
  ( -- * Creating a Request
    GetAttributeValues (..),
    newGetAttributeValues,

    -- * Request Lenses
    getAttributeValues_maxResults,
    getAttributeValues_nextToken,
    getAttributeValues_serviceCode,
    getAttributeValues_attributeName,

    -- * Destructuring the Response
    GetAttributeValuesResponse (..),
    newGetAttributeValuesResponse,

    -- * Response Lenses
    getAttributeValuesResponse_attributeValues,
    getAttributeValuesResponse_nextToken,
    getAttributeValuesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Pricing.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAttributeValues' smart constructor.
data GetAttributeValues = GetAttributeValues'
  { -- | The maximum number of results to return in response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The service code for the service whose attributes you want to retrieve.
    -- For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@.
    serviceCode :: Prelude.Text,
    -- | The name of the attribute that you want to retrieve the values for, such
    -- as @volumeType@.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributeValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getAttributeValues_maxResults' - The maximum number of results to return in response.
--
-- 'nextToken', 'getAttributeValues_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'serviceCode', 'getAttributeValues_serviceCode' - The service code for the service whose attributes you want to retrieve.
-- For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@.
--
-- 'attributeName', 'getAttributeValues_attributeName' - The name of the attribute that you want to retrieve the values for, such
-- as @volumeType@.
newGetAttributeValues ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  GetAttributeValues
newGetAttributeValues pServiceCode_ pAttributeName_ =
  GetAttributeValues'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceCode = pServiceCode_,
      attributeName = pAttributeName_
    }

-- | The maximum number of results to return in response.
getAttributeValues_maxResults :: Lens.Lens' GetAttributeValues (Prelude.Maybe Prelude.Natural)
getAttributeValues_maxResults = Lens.lens (\GetAttributeValues' {maxResults} -> maxResults) (\s@GetAttributeValues' {} a -> s {maxResults = a} :: GetAttributeValues)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getAttributeValues_nextToken :: Lens.Lens' GetAttributeValues (Prelude.Maybe Prelude.Text)
getAttributeValues_nextToken = Lens.lens (\GetAttributeValues' {nextToken} -> nextToken) (\s@GetAttributeValues' {} a -> s {nextToken = a} :: GetAttributeValues)

-- | The service code for the service whose attributes you want to retrieve.
-- For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@.
getAttributeValues_serviceCode :: Lens.Lens' GetAttributeValues Prelude.Text
getAttributeValues_serviceCode = Lens.lens (\GetAttributeValues' {serviceCode} -> serviceCode) (\s@GetAttributeValues' {} a -> s {serviceCode = a} :: GetAttributeValues)

-- | The name of the attribute that you want to retrieve the values for, such
-- as @volumeType@.
getAttributeValues_attributeName :: Lens.Lens' GetAttributeValues Prelude.Text
getAttributeValues_attributeName = Lens.lens (\GetAttributeValues' {attributeName} -> attributeName) (\s@GetAttributeValues' {} a -> s {attributeName = a} :: GetAttributeValues)

instance Core.AWSPager GetAttributeValues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAttributeValuesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getAttributeValuesResponse_attributeValues
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getAttributeValues_nextToken
              Lens..~ rs
              Lens.^? getAttributeValuesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetAttributeValues where
  type
    AWSResponse GetAttributeValues =
      GetAttributeValuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAttributeValuesResponse'
            Prelude.<$> ( x
                            Data..?> "AttributeValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttributeValues where
  hashWithSalt _salt GetAttributeValues' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData GetAttributeValues where
  rnf GetAttributeValues' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf serviceCode `Prelude.seq`
          Prelude.rnf attributeName

instance Data.ToHeaders GetAttributeValues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPriceListService.GetAttributeValues" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAttributeValues where
  toJSON GetAttributeValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceCode" Data..= serviceCode),
            Prelude.Just
              ("AttributeName" Data..= attributeName)
          ]
      )

instance Data.ToPath GetAttributeValues where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAttributeValues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAttributeValuesResponse' smart constructor.
data GetAttributeValuesResponse = GetAttributeValuesResponse'
  { -- | The list of values for an attribute. For example,
    -- @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available
    -- values for the @AmazonEC2@ @volumeType@.
    attributeValues :: Prelude.Maybe [AttributeValue],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributeValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValues', 'getAttributeValuesResponse_attributeValues' - The list of values for an attribute. For example,
-- @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available
-- values for the @AmazonEC2@ @volumeType@.
--
-- 'nextToken', 'getAttributeValuesResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'getAttributeValuesResponse_httpStatus' - The response's http status code.
newGetAttributeValuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAttributeValuesResponse
newGetAttributeValuesResponse pHttpStatus_ =
  GetAttributeValuesResponse'
    { attributeValues =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of values for an attribute. For example,
-- @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available
-- values for the @AmazonEC2@ @volumeType@.
getAttributeValuesResponse_attributeValues :: Lens.Lens' GetAttributeValuesResponse (Prelude.Maybe [AttributeValue])
getAttributeValuesResponse_attributeValues = Lens.lens (\GetAttributeValuesResponse' {attributeValues} -> attributeValues) (\s@GetAttributeValuesResponse' {} a -> s {attributeValues = a} :: GetAttributeValuesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
getAttributeValuesResponse_nextToken :: Lens.Lens' GetAttributeValuesResponse (Prelude.Maybe Prelude.Text)
getAttributeValuesResponse_nextToken = Lens.lens (\GetAttributeValuesResponse' {nextToken} -> nextToken) (\s@GetAttributeValuesResponse' {} a -> s {nextToken = a} :: GetAttributeValuesResponse)

-- | The response's http status code.
getAttributeValuesResponse_httpStatus :: Lens.Lens' GetAttributeValuesResponse Prelude.Int
getAttributeValuesResponse_httpStatus = Lens.lens (\GetAttributeValuesResponse' {httpStatus} -> httpStatus) (\s@GetAttributeValuesResponse' {} a -> s {httpStatus = a} :: GetAttributeValuesResponse)

instance Prelude.NFData GetAttributeValuesResponse where
  rnf GetAttributeValuesResponse' {..} =
    Prelude.rnf attributeValues `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
