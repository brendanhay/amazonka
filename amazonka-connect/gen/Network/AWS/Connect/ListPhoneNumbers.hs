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
-- Module      : Network.AWS.Connect.ListPhoneNumbers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the phone numbers for the specified Amazon
-- Connect instance.
--
-- For more information about phone numbers, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html Set Up Phone Numbers for Your Contact Center>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPhoneNumbers
  ( -- * Creating a Request
    ListPhoneNumbers (..),
    newListPhoneNumbers,

    -- * Request Lenses
    listPhoneNumbers_nextToken,
    listPhoneNumbers_phoneNumberTypes,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_phoneNumberCountryCodes,
    listPhoneNumbers_instanceId,

    -- * Destructuring the Response
    ListPhoneNumbersResponse (..),
    newListPhoneNumbersResponse,

    -- * Response Lenses
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumberSummaryList,
    listPhoneNumbersResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPhoneNumbers' smart constructor.
data ListPhoneNumbers = ListPhoneNumbers'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of phone number.
    phoneNumberTypes :: Core.Maybe [PhoneNumberType],
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ISO country code.
    phoneNumberCountryCodes :: Core.Maybe [PhoneNumberCountryCode],
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPhoneNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPhoneNumbers_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'phoneNumberTypes', 'listPhoneNumbers_phoneNumberTypes' - The type of phone number.
--
-- 'maxResults', 'listPhoneNumbers_maxResults' - The maximum number of results to return per page.
--
-- 'phoneNumberCountryCodes', 'listPhoneNumbers_phoneNumberCountryCodes' - The ISO country code.
--
-- 'instanceId', 'listPhoneNumbers_instanceId' - The identifier of the Amazon Connect instance.
newListPhoneNumbers ::
  -- | 'instanceId'
  Core.Text ->
  ListPhoneNumbers
newListPhoneNumbers pInstanceId_ =
  ListPhoneNumbers'
    { nextToken = Core.Nothing,
      phoneNumberTypes = Core.Nothing,
      maxResults = Core.Nothing,
      phoneNumberCountryCodes = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPhoneNumbers_nextToken :: Lens.Lens' ListPhoneNumbers (Core.Maybe Core.Text)
listPhoneNumbers_nextToken = Lens.lens (\ListPhoneNumbers' {nextToken} -> nextToken) (\s@ListPhoneNumbers' {} a -> s {nextToken = a} :: ListPhoneNumbers)

-- | The type of phone number.
listPhoneNumbers_phoneNumberTypes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [PhoneNumberType])
listPhoneNumbers_phoneNumberTypes = Lens.lens (\ListPhoneNumbers' {phoneNumberTypes} -> phoneNumberTypes) (\s@ListPhoneNumbers' {} a -> s {phoneNumberTypes = a} :: ListPhoneNumbers) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return per page.
listPhoneNumbers_maxResults :: Lens.Lens' ListPhoneNumbers (Core.Maybe Core.Natural)
listPhoneNumbers_maxResults = Lens.lens (\ListPhoneNumbers' {maxResults} -> maxResults) (\s@ListPhoneNumbers' {} a -> s {maxResults = a} :: ListPhoneNumbers)

-- | The ISO country code.
listPhoneNumbers_phoneNumberCountryCodes :: Lens.Lens' ListPhoneNumbers (Core.Maybe [PhoneNumberCountryCode])
listPhoneNumbers_phoneNumberCountryCodes = Lens.lens (\ListPhoneNumbers' {phoneNumberCountryCodes} -> phoneNumberCountryCodes) (\s@ListPhoneNumbers' {} a -> s {phoneNumberCountryCodes = a} :: ListPhoneNumbers) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon Connect instance.
listPhoneNumbers_instanceId :: Lens.Lens' ListPhoneNumbers Core.Text
listPhoneNumbers_instanceId = Lens.lens (\ListPhoneNumbers' {instanceId} -> instanceId) (\s@ListPhoneNumbers' {} a -> s {instanceId = a} :: ListPhoneNumbers)

instance Core.AWSPager ListPhoneNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPhoneNumbersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPhoneNumbersResponse_phoneNumberSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPhoneNumbers_nextToken
          Lens..~ rs
          Lens.^? listPhoneNumbersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPhoneNumbers where
  type
    AWSResponse ListPhoneNumbers =
      ListPhoneNumbersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPhoneNumbersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PhoneNumberSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPhoneNumbers

instance Core.NFData ListPhoneNumbers

instance Core.ToHeaders ListPhoneNumbers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListPhoneNumbers where
  toPath ListPhoneNumbers' {..} =
    Core.mconcat
      ["/phone-numbers-summary/", Core.toBS instanceId]

instance Core.ToQuery ListPhoneNumbers where
  toQuery ListPhoneNumbers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "phoneNumberTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> phoneNumberTypes
            ),
        "maxResults" Core.=: maxResults,
        "phoneNumberCountryCodes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> phoneNumberCountryCodes
            )
      ]

-- | /See:/ 'newListPhoneNumbersResponse' smart constructor.
data ListPhoneNumbersResponse = ListPhoneNumbersResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the phone numbers.
    phoneNumberSummaryList :: Core.Maybe [PhoneNumberSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPhoneNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPhoneNumbersResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'phoneNumberSummaryList', 'listPhoneNumbersResponse_phoneNumberSummaryList' - Information about the phone numbers.
--
-- 'httpStatus', 'listPhoneNumbersResponse_httpStatus' - The response's http status code.
newListPhoneNumbersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPhoneNumbersResponse
newListPhoneNumbersResponse pHttpStatus_ =
  ListPhoneNumbersResponse'
    { nextToken = Core.Nothing,
      phoneNumberSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listPhoneNumbersResponse_nextToken :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe Core.Text)
listPhoneNumbersResponse_nextToken = Lens.lens (\ListPhoneNumbersResponse' {nextToken} -> nextToken) (\s@ListPhoneNumbersResponse' {} a -> s {nextToken = a} :: ListPhoneNumbersResponse)

-- | Information about the phone numbers.
listPhoneNumbersResponse_phoneNumberSummaryList :: Lens.Lens' ListPhoneNumbersResponse (Core.Maybe [PhoneNumberSummary])
listPhoneNumbersResponse_phoneNumberSummaryList = Lens.lens (\ListPhoneNumbersResponse' {phoneNumberSummaryList} -> phoneNumberSummaryList) (\s@ListPhoneNumbersResponse' {} a -> s {phoneNumberSummaryList = a} :: ListPhoneNumbersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPhoneNumbersResponse_httpStatus :: Lens.Lens' ListPhoneNumbersResponse Core.Int
listPhoneNumbersResponse_httpStatus = Lens.lens (\ListPhoneNumbersResponse' {httpStatus} -> httpStatus) (\s@ListPhoneNumbersResponse' {} a -> s {httpStatus = a} :: ListPhoneNumbersResponse)

instance Core.NFData ListPhoneNumbersResponse
