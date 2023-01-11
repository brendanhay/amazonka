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
-- Module      : Amazonka.SNS.ListOriginationNumbers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the calling Amazon Web Services account\'s dedicated origination
-- numbers and their metadata. For more information about origination
-- numbers, see
-- <https://docs.aws.amazon.com/sns/latest/dg/channels-sms-originating-identities-origination-numbers.html Origination numbers>
-- in the /Amazon SNS Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SNS.ListOriginationNumbers
  ( -- * Creating a Request
    ListOriginationNumbers (..),
    newListOriginationNumbers,

    -- * Request Lenses
    listOriginationNumbers_maxResults,
    listOriginationNumbers_nextToken,

    -- * Destructuring the Response
    ListOriginationNumbersResponse (..),
    newListOriginationNumbersResponse,

    -- * Response Lenses
    listOriginationNumbersResponse_nextToken,
    listOriginationNumbersResponse_phoneNumbers,
    listOriginationNumbersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newListOriginationNumbers' smart constructor.
data ListOriginationNumbers = ListOriginationNumbers'
  { -- | The maximum number of origination numbers to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Token that the previous @ListOriginationNumbers@ request returns.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginationNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOriginationNumbers_maxResults' - The maximum number of origination numbers to return.
--
-- 'nextToken', 'listOriginationNumbers_nextToken' - Token that the previous @ListOriginationNumbers@ request returns.
newListOriginationNumbers ::
  ListOriginationNumbers
newListOriginationNumbers =
  ListOriginationNumbers'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of origination numbers to return.
listOriginationNumbers_maxResults :: Lens.Lens' ListOriginationNumbers (Prelude.Maybe Prelude.Natural)
listOriginationNumbers_maxResults = Lens.lens (\ListOriginationNumbers' {maxResults} -> maxResults) (\s@ListOriginationNumbers' {} a -> s {maxResults = a} :: ListOriginationNumbers)

-- | Token that the previous @ListOriginationNumbers@ request returns.
listOriginationNumbers_nextToken :: Lens.Lens' ListOriginationNumbers (Prelude.Maybe Prelude.Text)
listOriginationNumbers_nextToken = Lens.lens (\ListOriginationNumbers' {nextToken} -> nextToken) (\s@ListOriginationNumbers' {} a -> s {nextToken = a} :: ListOriginationNumbers)

instance Core.AWSPager ListOriginationNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOriginationNumbersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOriginationNumbersResponse_phoneNumbers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOriginationNumbers_nextToken
          Lens..~ rs
          Lens.^? listOriginationNumbersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOriginationNumbers where
  type
    AWSResponse ListOriginationNumbers =
      ListOriginationNumbersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListOriginationNumbersResult"
      ( \s h x ->
          ListOriginationNumbersResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "PhoneNumbers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOriginationNumbers where
  hashWithSalt _salt ListOriginationNumbers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOriginationNumbers where
  rnf ListOriginationNumbers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListOriginationNumbers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListOriginationNumbers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOriginationNumbers where
  toQuery ListOriginationNumbers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListOriginationNumbers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListOriginationNumbersResponse' smart constructor.
data ListOriginationNumbersResponse = ListOriginationNumbersResponse'
  { -- | A @NextToken@ string is returned when you call the
    -- @ListOriginationNumbers@ operation if additional pages of records are
    -- available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the calling account\'s verified and pending origination
    -- numbers.
    phoneNumbers :: Prelude.Maybe [PhoneNumberInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOriginationNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOriginationNumbersResponse_nextToken' - A @NextToken@ string is returned when you call the
-- @ListOriginationNumbers@ operation if additional pages of records are
-- available.
--
-- 'phoneNumbers', 'listOriginationNumbersResponse_phoneNumbers' - A list of the calling account\'s verified and pending origination
-- numbers.
--
-- 'httpStatus', 'listOriginationNumbersResponse_httpStatus' - The response's http status code.
newListOriginationNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOriginationNumbersResponse
newListOriginationNumbersResponse pHttpStatus_ =
  ListOriginationNumbersResponse'
    { nextToken =
        Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @NextToken@ string is returned when you call the
-- @ListOriginationNumbers@ operation if additional pages of records are
-- available.
listOriginationNumbersResponse_nextToken :: Lens.Lens' ListOriginationNumbersResponse (Prelude.Maybe Prelude.Text)
listOriginationNumbersResponse_nextToken = Lens.lens (\ListOriginationNumbersResponse' {nextToken} -> nextToken) (\s@ListOriginationNumbersResponse' {} a -> s {nextToken = a} :: ListOriginationNumbersResponse)

-- | A list of the calling account\'s verified and pending origination
-- numbers.
listOriginationNumbersResponse_phoneNumbers :: Lens.Lens' ListOriginationNumbersResponse (Prelude.Maybe [PhoneNumberInformation])
listOriginationNumbersResponse_phoneNumbers = Lens.lens (\ListOriginationNumbersResponse' {phoneNumbers} -> phoneNumbers) (\s@ListOriginationNumbersResponse' {} a -> s {phoneNumbers = a} :: ListOriginationNumbersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOriginationNumbersResponse_httpStatus :: Lens.Lens' ListOriginationNumbersResponse Prelude.Int
listOriginationNumbersResponse_httpStatus = Lens.lens (\ListOriginationNumbersResponse' {httpStatus} -> httpStatus) (\s@ListOriginationNumbersResponse' {} a -> s {httpStatus = a} :: ListOriginationNumbersResponse)

instance
  Prelude.NFData
    ListOriginationNumbersResponse
  where
  rnf ListOriginationNumbersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf httpStatus
