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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeOptedOutNumbers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified opted out destination numbers or all opted out
-- destination numbers in an opt-out list.
--
-- If you specify opted out numbers, the output includes information for
-- only the specified opted out numbers. If you specify filters, the output
-- includes information for only those opted out numbers that meet the
-- filter criteria. If you don\'t specify opted out numbers or filters, the
-- output includes information for all opted out destination numbers in
-- your opt-out list.
--
-- If you specify an opted out number that isn\'t valid, an Error is
-- returned.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeOptedOutNumbers
  ( -- * Creating a Request
    DescribeOptedOutNumbers (..),
    newDescribeOptedOutNumbers,

    -- * Request Lenses
    describeOptedOutNumbers_nextToken,
    describeOptedOutNumbers_optedOutNumbers,
    describeOptedOutNumbers_filters,
    describeOptedOutNumbers_maxResults,
    describeOptedOutNumbers_optOutListName,

    -- * Destructuring the Response
    DescribeOptedOutNumbersResponse (..),
    newDescribeOptedOutNumbersResponse,

    -- * Response Lenses
    describeOptedOutNumbersResponse_nextToken,
    describeOptedOutNumbersResponse_optOutListArn,
    describeOptedOutNumbersResponse_optedOutNumbers,
    describeOptedOutNumbersResponse_optOutListName,
    describeOptedOutNumbersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOptedOutNumbers' smart constructor.
data DescribeOptedOutNumbers = DescribeOptedOutNumbers'
  { -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of phone numbers to search for in the OptOutList.
    optedOutNumbers :: Prelude.Maybe [Prelude.Text],
    -- | An array of OptedOutFilter objects to filter the results on.
    filters :: Prelude.Maybe [OptedOutFilter],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The OptOutListName or OptOutListArn of the OptOutList. You can use
    -- DescribeOptOutLists to find the values for OptOutListName and
    -- OptOutListArn.
    optOutListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptedOutNumbers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOptedOutNumbers_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'optedOutNumbers', 'describeOptedOutNumbers_optedOutNumbers' - An array of phone numbers to search for in the OptOutList.
--
-- 'filters', 'describeOptedOutNumbers_filters' - An array of OptedOutFilter objects to filter the results on.
--
-- 'maxResults', 'describeOptedOutNumbers_maxResults' - The maximum number of results to return per each request.
--
-- 'optOutListName', 'describeOptedOutNumbers_optOutListName' - The OptOutListName or OptOutListArn of the OptOutList. You can use
-- DescribeOptOutLists to find the values for OptOutListName and
-- OptOutListArn.
newDescribeOptedOutNumbers ::
  -- | 'optOutListName'
  Prelude.Text ->
  DescribeOptedOutNumbers
newDescribeOptedOutNumbers pOptOutListName_ =
  DescribeOptedOutNumbers'
    { nextToken =
        Prelude.Nothing,
      optedOutNumbers = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      optOutListName = pOptOutListName_
    }

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeOptedOutNumbers_nextToken :: Lens.Lens' DescribeOptedOutNumbers (Prelude.Maybe Prelude.Text)
describeOptedOutNumbers_nextToken = Lens.lens (\DescribeOptedOutNumbers' {nextToken} -> nextToken) (\s@DescribeOptedOutNumbers' {} a -> s {nextToken = a} :: DescribeOptedOutNumbers)

-- | An array of phone numbers to search for in the OptOutList.
describeOptedOutNumbers_optedOutNumbers :: Lens.Lens' DescribeOptedOutNumbers (Prelude.Maybe [Prelude.Text])
describeOptedOutNumbers_optedOutNumbers = Lens.lens (\DescribeOptedOutNumbers' {optedOutNumbers} -> optedOutNumbers) (\s@DescribeOptedOutNumbers' {} a -> s {optedOutNumbers = a} :: DescribeOptedOutNumbers) Prelude.. Lens.mapping Lens.coerced

-- | An array of OptedOutFilter objects to filter the results on.
describeOptedOutNumbers_filters :: Lens.Lens' DescribeOptedOutNumbers (Prelude.Maybe [OptedOutFilter])
describeOptedOutNumbers_filters = Lens.lens (\DescribeOptedOutNumbers' {filters} -> filters) (\s@DescribeOptedOutNumbers' {} a -> s {filters = a} :: DescribeOptedOutNumbers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
describeOptedOutNumbers_maxResults :: Lens.Lens' DescribeOptedOutNumbers (Prelude.Maybe Prelude.Natural)
describeOptedOutNumbers_maxResults = Lens.lens (\DescribeOptedOutNumbers' {maxResults} -> maxResults) (\s@DescribeOptedOutNumbers' {} a -> s {maxResults = a} :: DescribeOptedOutNumbers)

-- | The OptOutListName or OptOutListArn of the OptOutList. You can use
-- DescribeOptOutLists to find the values for OptOutListName and
-- OptOutListArn.
describeOptedOutNumbers_optOutListName :: Lens.Lens' DescribeOptedOutNumbers Prelude.Text
describeOptedOutNumbers_optOutListName = Lens.lens (\DescribeOptedOutNumbers' {optOutListName} -> optOutListName) (\s@DescribeOptedOutNumbers' {} a -> s {optOutListName = a} :: DescribeOptedOutNumbers)

instance Core.AWSPager DescribeOptedOutNumbers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOptedOutNumbersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOptedOutNumbersResponse_optedOutNumbers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOptedOutNumbers_nextToken
          Lens..~ rs
          Lens.^? describeOptedOutNumbersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeOptedOutNumbers where
  type
    AWSResponse DescribeOptedOutNumbers =
      DescribeOptedOutNumbersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOptedOutNumbersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "OptOutListArn")
            Prelude.<*> ( x Data..?> "OptedOutNumbers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOptedOutNumbers where
  hashWithSalt _salt DescribeOptedOutNumbers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` optedOutNumbers
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` optOutListName

instance Prelude.NFData DescribeOptedOutNumbers where
  rnf DescribeOptedOutNumbers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf optedOutNumbers
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf optOutListName

instance Data.ToHeaders DescribeOptedOutNumbers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeOptedOutNumbers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOptedOutNumbers where
  toJSON DescribeOptedOutNumbers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OptedOutNumbers" Data..=)
              Prelude.<$> optedOutNumbers,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OptOutListName" Data..= optOutListName)
          ]
      )

instance Data.ToPath DescribeOptedOutNumbers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOptedOutNumbers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOptedOutNumbersResponse' smart constructor.
data DescribeOptedOutNumbersResponse = DescribeOptedOutNumbersResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the OptOutList.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | An array of OptedOutNumbersInformation objects that provide information
    -- about the requested OptedOutNumbers.
    optedOutNumbers :: Prelude.Maybe [OptedOutNumberInformation],
    -- | The name of the OptOutList.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptedOutNumbersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOptedOutNumbersResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'optOutListArn', 'describeOptedOutNumbersResponse_optOutListArn' - The Amazon Resource Name (ARN) of the OptOutList.
--
-- 'optedOutNumbers', 'describeOptedOutNumbersResponse_optedOutNumbers' - An array of OptedOutNumbersInformation objects that provide information
-- about the requested OptedOutNumbers.
--
-- 'optOutListName', 'describeOptedOutNumbersResponse_optOutListName' - The name of the OptOutList.
--
-- 'httpStatus', 'describeOptedOutNumbersResponse_httpStatus' - The response's http status code.
newDescribeOptedOutNumbersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOptedOutNumbersResponse
newDescribeOptedOutNumbersResponse pHttpStatus_ =
  DescribeOptedOutNumbersResponse'
    { nextToken =
        Prelude.Nothing,
      optOutListArn = Prelude.Nothing,
      optedOutNumbers = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeOptedOutNumbersResponse_nextToken :: Lens.Lens' DescribeOptedOutNumbersResponse (Prelude.Maybe Prelude.Text)
describeOptedOutNumbersResponse_nextToken = Lens.lens (\DescribeOptedOutNumbersResponse' {nextToken} -> nextToken) (\s@DescribeOptedOutNumbersResponse' {} a -> s {nextToken = a} :: DescribeOptedOutNumbersResponse)

-- | The Amazon Resource Name (ARN) of the OptOutList.
describeOptedOutNumbersResponse_optOutListArn :: Lens.Lens' DescribeOptedOutNumbersResponse (Prelude.Maybe Prelude.Text)
describeOptedOutNumbersResponse_optOutListArn = Lens.lens (\DescribeOptedOutNumbersResponse' {optOutListArn} -> optOutListArn) (\s@DescribeOptedOutNumbersResponse' {} a -> s {optOutListArn = a} :: DescribeOptedOutNumbersResponse)

-- | An array of OptedOutNumbersInformation objects that provide information
-- about the requested OptedOutNumbers.
describeOptedOutNumbersResponse_optedOutNumbers :: Lens.Lens' DescribeOptedOutNumbersResponse (Prelude.Maybe [OptedOutNumberInformation])
describeOptedOutNumbersResponse_optedOutNumbers = Lens.lens (\DescribeOptedOutNumbersResponse' {optedOutNumbers} -> optedOutNumbers) (\s@DescribeOptedOutNumbersResponse' {} a -> s {optedOutNumbers = a} :: DescribeOptedOutNumbersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the OptOutList.
describeOptedOutNumbersResponse_optOutListName :: Lens.Lens' DescribeOptedOutNumbersResponse (Prelude.Maybe Prelude.Text)
describeOptedOutNumbersResponse_optOutListName = Lens.lens (\DescribeOptedOutNumbersResponse' {optOutListName} -> optOutListName) (\s@DescribeOptedOutNumbersResponse' {} a -> s {optOutListName = a} :: DescribeOptedOutNumbersResponse)

-- | The response's http status code.
describeOptedOutNumbersResponse_httpStatus :: Lens.Lens' DescribeOptedOutNumbersResponse Prelude.Int
describeOptedOutNumbersResponse_httpStatus = Lens.lens (\DescribeOptedOutNumbersResponse' {httpStatus} -> httpStatus) (\s@DescribeOptedOutNumbersResponse' {} a -> s {httpStatus = a} :: DescribeOptedOutNumbersResponse)

instance
  Prelude.NFData
    DescribeOptedOutNumbersResponse
  where
  rnf DescribeOptedOutNumbersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf optedOutNumbers
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf httpStatus
