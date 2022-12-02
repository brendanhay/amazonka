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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeAccountLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Amazon Pinpoint SMS Voice V2 resource quotas for
-- your account. The description for a quota includes the quota name,
-- current usage toward that quota, and the quota\'s maximum value.
--
-- When you establish an Amazon Web Services account, the account has
-- initial quotas on the maximum number of configuration sets, opt-out
-- lists, phone numbers, and pools that you can create in a given Region.
-- For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/quotas.html Amazon Pinpoint quotas>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Request Lenses
    describeAccountLimits_nextToken,
    describeAccountLimits_maxResults,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccountLimits_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'maxResults', 'describeAccountLimits_maxResults' - The maximum number of results to return per each request.
newDescribeAccountLimits ::
  DescribeAccountLimits
newDescribeAccountLimits =
  DescribeAccountLimits'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeAccountLimits_nextToken :: Lens.Lens' DescribeAccountLimits (Prelude.Maybe Prelude.Text)
describeAccountLimits_nextToken = Lens.lens (\DescribeAccountLimits' {nextToken} -> nextToken) (\s@DescribeAccountLimits' {} a -> s {nextToken = a} :: DescribeAccountLimits)

-- | The maximum number of results to return per each request.
describeAccountLimits_maxResults :: Lens.Lens' DescribeAccountLimits (Prelude.Maybe Prelude.Natural)
describeAccountLimits_maxResults = Lens.lens (\DescribeAccountLimits' {maxResults} -> maxResults) (\s@DescribeAccountLimits' {} a -> s {maxResults = a} :: DescribeAccountLimits)

instance Core.AWSPager DescribeAccountLimits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_accountLimits
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAccountLimits_nextToken
          Lens..~ rs
          Lens.^? describeAccountLimitsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAccountLimits where
  type
    AWSResponse DescribeAccountLimits =
      DescribeAccountLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "AccountLimits" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits where
  hashWithSalt _salt DescribeAccountLimits' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeAccountLimits where
  rnf DescribeAccountLimits' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeAccountLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeAccountLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccountLimits where
  toJSON DescribeAccountLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of AccountLimit objects that show the current spend limits.
    accountLimits :: Prelude.Maybe [AccountLimit],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccountLimitsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'accountLimits', 'describeAccountLimitsResponse_accountLimits' - An array of AccountLimit objects that show the current spend limits.
--
-- 'httpStatus', 'describeAccountLimitsResponse_httpStatus' - The response's http status code.
newDescribeAccountLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountLimitsResponse
newDescribeAccountLimitsResponse pHttpStatus_ =
  DescribeAccountLimitsResponse'
    { nextToken =
        Prelude.Nothing,
      accountLimits = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeAccountLimitsResponse_nextToken :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Text)
describeAccountLimitsResponse_nextToken = Lens.lens (\DescribeAccountLimitsResponse' {nextToken} -> nextToken) (\s@DescribeAccountLimitsResponse' {} a -> s {nextToken = a} :: DescribeAccountLimitsResponse)

-- | An array of AccountLimit objects that show the current spend limits.
describeAccountLimitsResponse_accountLimits :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe [AccountLimit])
describeAccountLimitsResponse_accountLimits = Lens.lens (\DescribeAccountLimitsResponse' {accountLimits} -> accountLimits) (\s@DescribeAccountLimitsResponse' {} a -> s {accountLimits = a} :: DescribeAccountLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccountLimitsResponse_httpStatus :: Lens.Lens' DescribeAccountLimitsResponse Prelude.Int
describeAccountLimitsResponse_httpStatus = Lens.lens (\DescribeAccountLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountLimitsResponse' {} a -> s {httpStatus = a} :: DescribeAccountLimitsResponse)

instance Prelude.NFData DescribeAccountLimitsResponse where
  rnf DescribeAccountLimitsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountLimits
      `Prelude.seq` Prelude.rnf httpStatus
