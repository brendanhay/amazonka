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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribePhoneNumbers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified origination phone number, or all the phone
-- numbers in your account.
--
-- If you specify phone number IDs, the output includes information for
-- only the specified phone numbers. If you specify filters, the output
-- includes information for only those phone numbers that meet the filter
-- criteria. If you don\'t specify phone number IDs or filters, the output
-- includes information for all phone numbers.
--
-- If you specify a phone number ID that isn\'t valid, an Error is
-- returned.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribePhoneNumbers
  ( -- * Creating a Request
    DescribePhoneNumbers (..),
    newDescribePhoneNumbers,

    -- * Request Lenses
    describePhoneNumbers_nextToken,
    describePhoneNumbers_phoneNumberIds,
    describePhoneNumbers_filters,
    describePhoneNumbers_maxResults,

    -- * Destructuring the Response
    DescribePhoneNumbersResponse (..),
    newDescribePhoneNumbersResponse,

    -- * Response Lenses
    describePhoneNumbersResponse_nextToken,
    describePhoneNumbersResponse_phoneNumbers,
    describePhoneNumbersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePhoneNumbers' smart constructor.
data DescribePhoneNumbers = DescribePhoneNumbers'
  { -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of phone numbers to find information about. This
    -- is an array of strings that can be either the PhoneNumberId or
    -- PhoneNumberArn.
    phoneNumberIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of PhoneNumberFilter objects to filter the results.
    filters :: Prelude.Maybe [PhoneNumberFilter],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePhoneNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePhoneNumbers_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'phoneNumberIds', 'describePhoneNumbers_phoneNumberIds' - The unique identifier of phone numbers to find information about. This
-- is an array of strings that can be either the PhoneNumberId or
-- PhoneNumberArn.
--
-- 'filters', 'describePhoneNumbers_filters' - An array of PhoneNumberFilter objects to filter the results.
--
-- 'maxResults', 'describePhoneNumbers_maxResults' - The maximum number of results to return per each request.
newDescribePhoneNumbers ::
  DescribePhoneNumbers
newDescribePhoneNumbers =
  DescribePhoneNumbers'
    { nextToken = Prelude.Nothing,
      phoneNumberIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describePhoneNumbers_nextToken :: Lens.Lens' DescribePhoneNumbers (Prelude.Maybe Prelude.Text)
describePhoneNumbers_nextToken = Lens.lens (\DescribePhoneNumbers' {nextToken} -> nextToken) (\s@DescribePhoneNumbers' {} a -> s {nextToken = a} :: DescribePhoneNumbers)

-- | The unique identifier of phone numbers to find information about. This
-- is an array of strings that can be either the PhoneNumberId or
-- PhoneNumberArn.
describePhoneNumbers_phoneNumberIds :: Lens.Lens' DescribePhoneNumbers (Prelude.Maybe [Prelude.Text])
describePhoneNumbers_phoneNumberIds = Lens.lens (\DescribePhoneNumbers' {phoneNumberIds} -> phoneNumberIds) (\s@DescribePhoneNumbers' {} a -> s {phoneNumberIds = a} :: DescribePhoneNumbers) Prelude.. Lens.mapping Lens.coerced

-- | An array of PhoneNumberFilter objects to filter the results.
describePhoneNumbers_filters :: Lens.Lens' DescribePhoneNumbers (Prelude.Maybe [PhoneNumberFilter])
describePhoneNumbers_filters = Lens.lens (\DescribePhoneNumbers' {filters} -> filters) (\s@DescribePhoneNumbers' {} a -> s {filters = a} :: DescribePhoneNumbers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
describePhoneNumbers_maxResults :: Lens.Lens' DescribePhoneNumbers (Prelude.Maybe Prelude.Natural)
describePhoneNumbers_maxResults = Lens.lens (\DescribePhoneNumbers' {maxResults} -> maxResults) (\s@DescribePhoneNumbers' {} a -> s {maxResults = a} :: DescribePhoneNumbers)

instance Core.AWSPager DescribePhoneNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePhoneNumbersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePhoneNumbersResponse_phoneNumbers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePhoneNumbers_nextToken
          Lens..~ rs
          Lens.^? describePhoneNumbersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePhoneNumbers where
  type
    AWSResponse DescribePhoneNumbers =
      DescribePhoneNumbersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePhoneNumbersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "PhoneNumbers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePhoneNumbers where
  hashWithSalt _salt DescribePhoneNumbers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` phoneNumberIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribePhoneNumbers where
  rnf DescribePhoneNumbers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumberIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribePhoneNumbers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PinpointSMSVoiceV2.DescribePhoneNumbers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePhoneNumbers where
  toJSON DescribePhoneNumbers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("PhoneNumberIds" Core..=)
              Prelude.<$> phoneNumberIds,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribePhoneNumbers where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePhoneNumbers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePhoneNumbersResponse' smart constructor.
data DescribePhoneNumbersResponse = DescribePhoneNumbersResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of PhoneNumberInformation objects that contain the details for
    -- the requested phone numbers.
    phoneNumbers :: Prelude.Maybe [PhoneNumberInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePhoneNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePhoneNumbersResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'phoneNumbers', 'describePhoneNumbersResponse_phoneNumbers' - An array of PhoneNumberInformation objects that contain the details for
-- the requested phone numbers.
--
-- 'httpStatus', 'describePhoneNumbersResponse_httpStatus' - The response's http status code.
newDescribePhoneNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePhoneNumbersResponse
newDescribePhoneNumbersResponse pHttpStatus_ =
  DescribePhoneNumbersResponse'
    { nextToken =
        Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describePhoneNumbersResponse_nextToken :: Lens.Lens' DescribePhoneNumbersResponse (Prelude.Maybe Prelude.Text)
describePhoneNumbersResponse_nextToken = Lens.lens (\DescribePhoneNumbersResponse' {nextToken} -> nextToken) (\s@DescribePhoneNumbersResponse' {} a -> s {nextToken = a} :: DescribePhoneNumbersResponse)

-- | An array of PhoneNumberInformation objects that contain the details for
-- the requested phone numbers.
describePhoneNumbersResponse_phoneNumbers :: Lens.Lens' DescribePhoneNumbersResponse (Prelude.Maybe [PhoneNumberInformation])
describePhoneNumbersResponse_phoneNumbers = Lens.lens (\DescribePhoneNumbersResponse' {phoneNumbers} -> phoneNumbers) (\s@DescribePhoneNumbersResponse' {} a -> s {phoneNumbers = a} :: DescribePhoneNumbersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePhoneNumbersResponse_httpStatus :: Lens.Lens' DescribePhoneNumbersResponse Prelude.Int
describePhoneNumbersResponse_httpStatus = Lens.lens (\DescribePhoneNumbersResponse' {httpStatus} -> httpStatus) (\s@DescribePhoneNumbersResponse' {} a -> s {httpStatus = a} :: DescribePhoneNumbersResponse)

instance Prelude.NFData DescribePhoneNumbersResponse where
  rnf DescribePhoneNumbersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf httpStatus
