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
-- Module      : Amazonka.Chime.SearchAvailablePhoneNumbers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for phone numbers that can be ordered. For US numbers, provide
-- at least one of the following search filters: @AreaCode@, @City@,
-- @State@, or @TollFreePrefix@. If you provide @City@, you must also
-- provide @State@. Numbers outside the US only support the
-- @PhoneNumberType@ filter, which you must use.
module Amazonka.Chime.SearchAvailablePhoneNumbers
  ( -- * Creating a Request
    SearchAvailablePhoneNumbers (..),
    newSearchAvailablePhoneNumbers,

    -- * Request Lenses
    searchAvailablePhoneNumbers_areaCode,
    searchAvailablePhoneNumbers_city,
    searchAvailablePhoneNumbers_country,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_phoneNumberType,
    searchAvailablePhoneNumbers_state,
    searchAvailablePhoneNumbers_tollFreePrefix,

    -- * Destructuring the Response
    SearchAvailablePhoneNumbersResponse (..),
    newSearchAvailablePhoneNumbersResponse,

    -- * Response Lenses
    searchAvailablePhoneNumbersResponse_e164PhoneNumbers,
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchAvailablePhoneNumbers' smart constructor.
data SearchAvailablePhoneNumbers = SearchAvailablePhoneNumbers'
  { -- | The area code used to filter results. Only applies to the US.
    areaCode :: Prelude.Maybe Prelude.Text,
    -- | The city used to filter results. Only applies to the US.
    city :: Prelude.Maybe Prelude.Text,
    -- | The country used to filter results. Defaults to the US Format: ISO
    -- 3166-1 alpha-2.
    country :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The phone number type used to filter results. Required for non-US
    -- numbers.
    phoneNumberType :: Prelude.Maybe PhoneNumberType,
    -- | The state used to filter results. Required only if you provide @City@.
    -- Only applies to the US.
    state :: Prelude.Maybe Prelude.Text,
    -- | The toll-free prefix that you use to filter results. Only applies to the
    -- US.
    tollFreePrefix :: Prelude.Maybe Prelude.Text
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
-- 'areaCode', 'searchAvailablePhoneNumbers_areaCode' - The area code used to filter results. Only applies to the US.
--
-- 'city', 'searchAvailablePhoneNumbers_city' - The city used to filter results. Only applies to the US.
--
-- 'country', 'searchAvailablePhoneNumbers_country' - The country used to filter results. Defaults to the US Format: ISO
-- 3166-1 alpha-2.
--
-- 'maxResults', 'searchAvailablePhoneNumbers_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'searchAvailablePhoneNumbers_nextToken' - The token used to retrieve the next page of results.
--
-- 'phoneNumberType', 'searchAvailablePhoneNumbers_phoneNumberType' - The phone number type used to filter results. Required for non-US
-- numbers.
--
-- 'state', 'searchAvailablePhoneNumbers_state' - The state used to filter results. Required only if you provide @City@.
-- Only applies to the US.
--
-- 'tollFreePrefix', 'searchAvailablePhoneNumbers_tollFreePrefix' - The toll-free prefix that you use to filter results. Only applies to the
-- US.
newSearchAvailablePhoneNumbers ::
  SearchAvailablePhoneNumbers
newSearchAvailablePhoneNumbers =
  SearchAvailablePhoneNumbers'
    { areaCode =
        Prelude.Nothing,
      city = Prelude.Nothing,
      country = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      phoneNumberType = Prelude.Nothing,
      state = Prelude.Nothing,
      tollFreePrefix = Prelude.Nothing
    }

-- | The area code used to filter results. Only applies to the US.
searchAvailablePhoneNumbers_areaCode :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_areaCode = Lens.lens (\SearchAvailablePhoneNumbers' {areaCode} -> areaCode) (\s@SearchAvailablePhoneNumbers' {} a -> s {areaCode = a} :: SearchAvailablePhoneNumbers)

-- | The city used to filter results. Only applies to the US.
searchAvailablePhoneNumbers_city :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_city = Lens.lens (\SearchAvailablePhoneNumbers' {city} -> city) (\s@SearchAvailablePhoneNumbers' {} a -> s {city = a} :: SearchAvailablePhoneNumbers)

-- | The country used to filter results. Defaults to the US Format: ISO
-- 3166-1 alpha-2.
searchAvailablePhoneNumbers_country :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_country = Lens.lens (\SearchAvailablePhoneNumbers' {country} -> country) (\s@SearchAvailablePhoneNumbers' {} a -> s {country = a} :: SearchAvailablePhoneNumbers)

-- | The maximum number of results to return in a single call.
searchAvailablePhoneNumbers_maxResults :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Natural)
searchAvailablePhoneNumbers_maxResults = Lens.lens (\SearchAvailablePhoneNumbers' {maxResults} -> maxResults) (\s@SearchAvailablePhoneNumbers' {} a -> s {maxResults = a} :: SearchAvailablePhoneNumbers)

-- | The token used to retrieve the next page of results.
searchAvailablePhoneNumbers_nextToken :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_nextToken = Lens.lens (\SearchAvailablePhoneNumbers' {nextToken} -> nextToken) (\s@SearchAvailablePhoneNumbers' {} a -> s {nextToken = a} :: SearchAvailablePhoneNumbers)

-- | The phone number type used to filter results. Required for non-US
-- numbers.
searchAvailablePhoneNumbers_phoneNumberType :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe PhoneNumberType)
searchAvailablePhoneNumbers_phoneNumberType = Lens.lens (\SearchAvailablePhoneNumbers' {phoneNumberType} -> phoneNumberType) (\s@SearchAvailablePhoneNumbers' {} a -> s {phoneNumberType = a} :: SearchAvailablePhoneNumbers)

-- | The state used to filter results. Required only if you provide @City@.
-- Only applies to the US.
searchAvailablePhoneNumbers_state :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_state = Lens.lens (\SearchAvailablePhoneNumbers' {state} -> state) (\s@SearchAvailablePhoneNumbers' {} a -> s {state = a} :: SearchAvailablePhoneNumbers)

-- | The toll-free prefix that you use to filter results. Only applies to the
-- US.
searchAvailablePhoneNumbers_tollFreePrefix :: Lens.Lens' SearchAvailablePhoneNumbers (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbers_tollFreePrefix = Lens.lens (\SearchAvailablePhoneNumbers' {tollFreePrefix} -> tollFreePrefix) (\s@SearchAvailablePhoneNumbers' {} a -> s {tollFreePrefix = a} :: SearchAvailablePhoneNumbers)

instance Core.AWSRequest SearchAvailablePhoneNumbers where
  type
    AWSResponse SearchAvailablePhoneNumbers =
      SearchAvailablePhoneNumbersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAvailablePhoneNumbersResponse'
            Prelude.<$> ( x Data..?> "E164PhoneNumbers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchAvailablePhoneNumbers where
  hashWithSalt _salt SearchAvailablePhoneNumbers' {..} =
    _salt `Prelude.hashWithSalt` areaCode
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` phoneNumberType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tollFreePrefix

instance Prelude.NFData SearchAvailablePhoneNumbers where
  rnf SearchAvailablePhoneNumbers' {..} =
    Prelude.rnf areaCode
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumberType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tollFreePrefix

instance Data.ToHeaders SearchAvailablePhoneNumbers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SearchAvailablePhoneNumbers where
  toPath = Prelude.const "/search"

instance Data.ToQuery SearchAvailablePhoneNumbers where
  toQuery SearchAvailablePhoneNumbers' {..} =
    Prelude.mconcat
      [ "area-code" Data.=: areaCode,
        "city" Data.=: city,
        "country" Data.=: country,
        "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "phone-number-type" Data.=: phoneNumberType,
        "state" Data.=: state,
        "toll-free-prefix" Data.=: tollFreePrefix,
        "type=phone-numbers"
      ]

-- | /See:/ 'newSearchAvailablePhoneNumbersResponse' smart constructor.
data SearchAvailablePhoneNumbersResponse = SearchAvailablePhoneNumbersResponse'
  { -- | List of phone numbers, in E.164 format.
    e164PhoneNumbers :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The token used to retrieve the next page of search results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAvailablePhoneNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'e164PhoneNumbers', 'searchAvailablePhoneNumbersResponse_e164PhoneNumbers' - List of phone numbers, in E.164 format.
--
-- 'nextToken', 'searchAvailablePhoneNumbersResponse_nextToken' - The token used to retrieve the next page of search results.
--
-- 'httpStatus', 'searchAvailablePhoneNumbersResponse_httpStatus' - The response's http status code.
newSearchAvailablePhoneNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchAvailablePhoneNumbersResponse
newSearchAvailablePhoneNumbersResponse pHttpStatus_ =
  SearchAvailablePhoneNumbersResponse'
    { e164PhoneNumbers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of phone numbers, in E.164 format.
searchAvailablePhoneNumbersResponse_e164PhoneNumbers :: Lens.Lens' SearchAvailablePhoneNumbersResponse (Prelude.Maybe [Prelude.Text])
searchAvailablePhoneNumbersResponse_e164PhoneNumbers = Lens.lens (\SearchAvailablePhoneNumbersResponse' {e164PhoneNumbers} -> e164PhoneNumbers) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {e164PhoneNumbers = a} :: SearchAvailablePhoneNumbersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of search results.
searchAvailablePhoneNumbersResponse_nextToken :: Lens.Lens' SearchAvailablePhoneNumbersResponse (Prelude.Maybe Prelude.Text)
searchAvailablePhoneNumbersResponse_nextToken = Lens.lens (\SearchAvailablePhoneNumbersResponse' {nextToken} -> nextToken) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {nextToken = a} :: SearchAvailablePhoneNumbersResponse)

-- | The response's http status code.
searchAvailablePhoneNumbersResponse_httpStatus :: Lens.Lens' SearchAvailablePhoneNumbersResponse Prelude.Int
searchAvailablePhoneNumbersResponse_httpStatus = Lens.lens (\SearchAvailablePhoneNumbersResponse' {httpStatus} -> httpStatus) (\s@SearchAvailablePhoneNumbersResponse' {} a -> s {httpStatus = a} :: SearchAvailablePhoneNumbersResponse)

instance
  Prelude.NFData
    SearchAvailablePhoneNumbersResponse
  where
  rnf SearchAvailablePhoneNumbersResponse' {..} =
    Prelude.rnf e164PhoneNumbers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
