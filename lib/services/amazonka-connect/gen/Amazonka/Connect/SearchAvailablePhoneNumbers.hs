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
-- Module      : Amazonka.Connect.SearchAvailablePhoneNumbers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for available phone numbers that you can claim to your Amazon
-- Connect instance or traffic distribution group. If the provided
-- @TargetArn@ is a traffic distribution group, you can call this API in
-- both Amazon Web Services Regions associated with the traffic
-- distribution group.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchAvailablePhoneNumbers
  ( -- * Creating a Request
    SearchAvailablePhoneNumbers (..),
    newSearchAvailablePhoneNumbers,

    -- * Request Lenses
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_phoneNumberPrefix,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbers_targetArn,
    searchAvailablePhoneNumbers_phoneNumberCountryCode,
    searchAvailablePhoneNumbers_phoneNumberType,

    -- * Destructuring the Response
    SearchAvailablePhoneNumbersResponse (..),
    newSearchAvailablePhoneNumbersResponse,

    -- * Response Lenses
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_availableNumbersList,
    searchAvailablePhoneNumbersResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchAvailablePhoneNumbers' smart constructor.
data SearchAvailablePhoneNumbers = SearchAvailablePhoneNumbers'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The prefix of the phone number. If provided, it must contain @+@ as part
    -- of the country code.
    phoneNumberPrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Text,
    -- | The ISO country code.
    phoneNumberCountryCode :: PhoneNumberCountryCode,
    -- | The type of phone number.
    phoneNumberType :: PhoneNumberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAvailablePhoneNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchAvailablePhoneNumbers_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'phoneNumberPrefix', 'searchAvailablePhoneNumbers_phoneNumberPrefix' - The prefix of the phone number. If provided, it must contain @+@ as part
-- of the country code.
--
-- 'maxResults', 'searchAvailablePhoneNumbers_maxResults' - The maximum number of results to return per page.
--
-- 'targetArn', 'searchAvailablePhoneNumbers_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
--
-- 'phoneNumberCountryCode', 'searchAvailablePhoneNumbers_phoneNumberCountryCode' - The ISO country code.
--
-- 'phoneNumberType', 'searchAvailablePhoneNumbers_phoneNumberType' - The type of phone number.
newSearchAvailablePhoneNumbers ::
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'phoneNumberCountryCode'
  PhoneNumberCountryCode ->
  -- | 'phoneNumberType'
  PhoneNumberType ->
  SearchAvailablePhoneNumbers
newSearchAvailablePhoneNumbers
  pTargetArn_
  pPhoneNumberCountryCode_
  pPhoneNumberType_ =
    SearchAvailablePhoneNumbers'
      { nextToken =
          Prelude.Nothing,
        phoneNumberPrefix = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        targetArn = pTargetArn_,
        phoneNumberCountryCode =
          pPhoneNumberCountryCode_,
        phoneNumberType = pPhoneNumberType_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchAvailablePhoneNumbers_nextToken :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_nextToken = Lens.lens (\SearchAvailablePhoneNumbers' {nextToken} -> nextToken) (\s@SearchAvailablePhoneNumbers' {} a -> s {nextToken = a} :: SearchAvailablePhoneNumbers)

-- | The prefix of the phone number. If provided, it must contain @+@ as part
-- of the country code.
searchAvailablePhoneNumbers_phoneNumberPrefix :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_phoneNumberPrefix = Lens.lens (\SearchAvailablePhoneNumbers' {phoneNumberPrefix} -> phoneNumberPrefix) (\s@SearchAvailablePhoneNumbers' {} a -> s {phoneNumberPrefix = a} :: SearchAvailablePhoneNumbers)

-- | The maximum number of results to return per page.
searchAvailablePhoneNumbers_maxResults :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Natural)
searchAvailablePhoneNumbers_maxResults = Lens.lens (\SearchAvailablePhoneNumbers' {maxResults} -> maxResults) (\s@SearchAvailablePhoneNumbers' {} a -> s {maxResults = a} :: SearchAvailablePhoneNumbers)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
searchAvailablePhoneNumbers_targetArn :: Lens.Lens' SearchAvailablePhoneNumbers Prelude.Text
searchAvailablePhoneNumbers_targetArn = Lens.lens (\SearchAvailablePhoneNumbers' {targetArn} -> targetArn) (\s@SearchAvailablePhoneNumbers' {} a -> s {targetArn = a} :: SearchAvailablePhoneNumbers)

-- | The ISO country code.
searchAvailablePhoneNumbers_phoneNumberCountryCode :: Lens.Lens' SearchAvailablePhoneNumbers PhoneNumberCountryCode
searchAvailablePhoneNumbers_phoneNumberCountryCode = Lens.lens (\SearchAvailablePhoneNumbers' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@SearchAvailablePhoneNumbers' {} a -> s {phoneNumberCountryCode = a} :: SearchAvailablePhoneNumbers)

-- | The type of phone number.
searchAvailablePhoneNumbers_phoneNumberType :: Lens.Lens' SearchAvailablePhoneNumbers PhoneNumberType
searchAvailablePhoneNumbers_phoneNumberType = Lens.lens (\SearchAvailablePhoneNumbers' {phoneNumberType} -> phoneNumberType) (\s@SearchAvailablePhoneNumbers' {} a -> s {phoneNumberType = a} :: SearchAvailablePhoneNumbers)

instance Core.AWSPager SearchAvailablePhoneNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchAvailablePhoneNumbersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchAvailablePhoneNumbersResponse_availableNumbersList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchAvailablePhoneNumbers_nextToken
          Lens..~ rs
          Lens.^? searchAvailablePhoneNumbersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchAvailablePhoneNumbers where
  type
    AWSResponse SearchAvailablePhoneNumbers =
      SearchAvailablePhoneNumbersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAvailablePhoneNumbersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "AvailableNumbersList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchAvailablePhoneNumbers where
  hashWithSalt _salt SearchAvailablePhoneNumbers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` phoneNumberPrefix
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` phoneNumberCountryCode
      `Prelude.hashWithSalt` phoneNumberType

instance Prelude.NFData SearchAvailablePhoneNumbers where
  rnf SearchAvailablePhoneNumbers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumberPrefix
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf phoneNumberCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberType

instance Core.ToHeaders SearchAvailablePhoneNumbers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchAvailablePhoneNumbers where
  toJSON SearchAvailablePhoneNumbers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("PhoneNumberPrefix" Core..=)
              Prelude.<$> phoneNumberPrefix,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("TargetArn" Core..= targetArn),
            Prelude.Just
              ( "PhoneNumberCountryCode"
                  Core..= phoneNumberCountryCode
              ),
            Prelude.Just
              ("PhoneNumberType" Core..= phoneNumberType)
          ]
      )

instance Core.ToPath SearchAvailablePhoneNumbers where
  toPath =
    Prelude.const "/phone-number/search-available"

instance Core.ToQuery SearchAvailablePhoneNumbers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchAvailablePhoneNumbersResponse' smart constructor.
data SearchAvailablePhoneNumbersResponse = SearchAvailablePhoneNumbersResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of available phone numbers that you can claim to your Amazon
    -- Connect instance or traffic distribution group.
    availableNumbersList :: Prelude.Maybe [AvailableNumberSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAvailablePhoneNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchAvailablePhoneNumbersResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'availableNumbersList', 'searchAvailablePhoneNumbersResponse_availableNumbersList' - A list of available phone numbers that you can claim to your Amazon
-- Connect instance or traffic distribution group.
--
-- 'httpStatus', 'searchAvailablePhoneNumbersResponse_httpStatus' - The response's http status code.
newSearchAvailablePhoneNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchAvailablePhoneNumbersResponse
newSearchAvailablePhoneNumbersResponse pHttpStatus_ =
  SearchAvailablePhoneNumbersResponse'
    { nextToken =
        Prelude.Nothing,
      availableNumbersList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchAvailablePhoneNumbersResponse_nextToken :: Lens.Lens' SearchAvailablePhoneNumbersResponse (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbersResponse_nextToken = Lens.lens (\SearchAvailablePhoneNumbersResponse' {nextToken} -> nextToken) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {nextToken = a} :: SearchAvailablePhoneNumbersResponse)

-- | A list of available phone numbers that you can claim to your Amazon
-- Connect instance or traffic distribution group.
searchAvailablePhoneNumbersResponse_availableNumbersList :: Lens.Lens' SearchAvailablePhoneNumbersResponse (Prelude.Maybe [AvailableNumberSummary])
searchAvailablePhoneNumbersResponse_availableNumbersList = Lens.lens (\SearchAvailablePhoneNumbersResponse' {availableNumbersList} -> availableNumbersList) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {availableNumbersList = a} :: SearchAvailablePhoneNumbersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchAvailablePhoneNumbersResponse_httpStatus :: Lens.Lens' SearchAvailablePhoneNumbersResponse Prelude.Int
searchAvailablePhoneNumbersResponse_httpStatus = Lens.lens (\SearchAvailablePhoneNumbersResponse' {httpStatus} -> httpStatus) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {httpStatus = a} :: SearchAvailablePhoneNumbersResponse)

instance
  Prelude.NFData
    SearchAvailablePhoneNumbersResponse
  where
  rnf SearchAvailablePhoneNumbersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf availableNumbersList
      `Prelude.seq` Prelude.rnf httpStatus
