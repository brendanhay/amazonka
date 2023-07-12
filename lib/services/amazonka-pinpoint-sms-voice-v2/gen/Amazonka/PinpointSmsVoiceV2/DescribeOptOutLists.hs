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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeOptOutLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified opt-out list or all opt-out lists in your
-- account.
--
-- If you specify opt-out list names, the output includes information for
-- only the specified opt-out lists. Opt-out lists include only those that
-- meet the filter criteria. If you don\'t specify opt-out list names or
-- filters, the output includes information for all opt-out lists.
--
-- If you specify an opt-out list name that isn\'t valid, an Error is
-- returned.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeOptOutLists
  ( -- * Creating a Request
    DescribeOptOutLists (..),
    newDescribeOptOutLists,

    -- * Request Lenses
    describeOptOutLists_maxResults,
    describeOptOutLists_nextToken,
    describeOptOutLists_optOutListNames,

    -- * Destructuring the Response
    DescribeOptOutListsResponse (..),
    newDescribeOptOutListsResponse,

    -- * Response Lenses
    describeOptOutListsResponse_nextToken,
    describeOptOutListsResponse_optOutLists,
    describeOptOutListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOptOutLists' smart constructor.
data DescribeOptOutLists = DescribeOptOutLists'
  { -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The OptOutLists to show the details of. This is an array of strings that
    -- can be either the OptOutListName or OptOutListArn.
    optOutListNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptOutLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeOptOutLists_maxResults' - The maximum number of results to return per each request.
--
-- 'nextToken', 'describeOptOutLists_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'optOutListNames', 'describeOptOutLists_optOutListNames' - The OptOutLists to show the details of. This is an array of strings that
-- can be either the OptOutListName or OptOutListArn.
newDescribeOptOutLists ::
  DescribeOptOutLists
newDescribeOptOutLists =
  DescribeOptOutLists'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      optOutListNames = Prelude.Nothing
    }

-- | The maximum number of results to return per each request.
describeOptOutLists_maxResults :: Lens.Lens' DescribeOptOutLists (Prelude.Maybe Prelude.Natural)
describeOptOutLists_maxResults = Lens.lens (\DescribeOptOutLists' {maxResults} -> maxResults) (\s@DescribeOptOutLists' {} a -> s {maxResults = a} :: DescribeOptOutLists)

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeOptOutLists_nextToken :: Lens.Lens' DescribeOptOutLists (Prelude.Maybe Prelude.Text)
describeOptOutLists_nextToken = Lens.lens (\DescribeOptOutLists' {nextToken} -> nextToken) (\s@DescribeOptOutLists' {} a -> s {nextToken = a} :: DescribeOptOutLists)

-- | The OptOutLists to show the details of. This is an array of strings that
-- can be either the OptOutListName or OptOutListArn.
describeOptOutLists_optOutListNames :: Lens.Lens' DescribeOptOutLists (Prelude.Maybe [Prelude.Text])
describeOptOutLists_optOutListNames = Lens.lens (\DescribeOptOutLists' {optOutListNames} -> optOutListNames) (\s@DescribeOptOutLists' {} a -> s {optOutListNames = a} :: DescribeOptOutLists) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeOptOutLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOptOutListsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOptOutListsResponse_optOutLists
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeOptOutLists_nextToken
          Lens..~ rs
          Lens.^? describeOptOutListsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeOptOutLists where
  type
    AWSResponse DescribeOptOutLists =
      DescribeOptOutListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOptOutListsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "OptOutLists" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOptOutLists where
  hashWithSalt _salt DescribeOptOutLists' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` optOutListNames

instance Prelude.NFData DescribeOptOutLists where
  rnf DescribeOptOutLists' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf optOutListNames

instance Data.ToHeaders DescribeOptOutLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeOptOutLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOptOutLists where
  toJSON DescribeOptOutLists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OptOutListNames" Data..=)
              Prelude.<$> optOutListNames
          ]
      )

instance Data.ToPath DescribeOptOutLists where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOptOutLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOptOutListsResponse' smart constructor.
data DescribeOptOutListsResponse = DescribeOptOutListsResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of OptOutListInformation objects that contain the details for
    -- the requested OptOutLists.
    optOutLists :: Prelude.Maybe [OptOutListInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptOutListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOptOutListsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'optOutLists', 'describeOptOutListsResponse_optOutLists' - An array of OptOutListInformation objects that contain the details for
-- the requested OptOutLists.
--
-- 'httpStatus', 'describeOptOutListsResponse_httpStatus' - The response's http status code.
newDescribeOptOutListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOptOutListsResponse
newDescribeOptOutListsResponse pHttpStatus_ =
  DescribeOptOutListsResponse'
    { nextToken =
        Prelude.Nothing,
      optOutLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeOptOutListsResponse_nextToken :: Lens.Lens' DescribeOptOutListsResponse (Prelude.Maybe Prelude.Text)
describeOptOutListsResponse_nextToken = Lens.lens (\DescribeOptOutListsResponse' {nextToken} -> nextToken) (\s@DescribeOptOutListsResponse' {} a -> s {nextToken = a} :: DescribeOptOutListsResponse)

-- | An array of OptOutListInformation objects that contain the details for
-- the requested OptOutLists.
describeOptOutListsResponse_optOutLists :: Lens.Lens' DescribeOptOutListsResponse (Prelude.Maybe [OptOutListInformation])
describeOptOutListsResponse_optOutLists = Lens.lens (\DescribeOptOutListsResponse' {optOutLists} -> optOutLists) (\s@DescribeOptOutListsResponse' {} a -> s {optOutLists = a} :: DescribeOptOutListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOptOutListsResponse_httpStatus :: Lens.Lens' DescribeOptOutListsResponse Prelude.Int
describeOptOutListsResponse_httpStatus = Lens.lens (\DescribeOptOutListsResponse' {httpStatus} -> httpStatus) (\s@DescribeOptOutListsResponse' {} a -> s {httpStatus = a} :: DescribeOptOutListsResponse)

instance Prelude.NFData DescribeOptOutListsResponse where
  rnf DescribeOptOutListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf optOutLists
      `Prelude.seq` Prelude.rnf httpStatus
