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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeSpendLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Amazon Pinpoint monthly spend limits for sending
-- voice and text messages.
--
-- When you establish an Amazon Web Services account, the account has
-- initial monthly spend limit in a given Region. For more information on
-- increasing your monthly spend limit, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-awssupport-spend-threshold.html Requesting increases to your monthly SMS spending quota for Amazon Pinpoint>
-- in the /Amazon Pinpoint User Guide/.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeSpendLimits
  ( -- * Creating a Request
    DescribeSpendLimits (..),
    newDescribeSpendLimits,

    -- * Request Lenses
    describeSpendLimits_nextToken,
    describeSpendLimits_maxResults,

    -- * Destructuring the Response
    DescribeSpendLimitsResponse (..),
    newDescribeSpendLimitsResponse,

    -- * Response Lenses
    describeSpendLimitsResponse_nextToken,
    describeSpendLimitsResponse_spendLimits,
    describeSpendLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSpendLimits' smart constructor.
data DescribeSpendLimits = DescribeSpendLimits'
  { -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpendLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpendLimits_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'maxResults', 'describeSpendLimits_maxResults' - The maximum number of results to return per each request.
newDescribeSpendLimits ::
  DescribeSpendLimits
newDescribeSpendLimits =
  DescribeSpendLimits'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeSpendLimits_nextToken :: Lens.Lens' DescribeSpendLimits (Prelude.Maybe Prelude.Text)
describeSpendLimits_nextToken = Lens.lens (\DescribeSpendLimits' {nextToken} -> nextToken) (\s@DescribeSpendLimits' {} a -> s {nextToken = a} :: DescribeSpendLimits)

-- | The maximum number of results to return per each request.
describeSpendLimits_maxResults :: Lens.Lens' DescribeSpendLimits (Prelude.Maybe Prelude.Natural)
describeSpendLimits_maxResults = Lens.lens (\DescribeSpendLimits' {maxResults} -> maxResults) (\s@DescribeSpendLimits' {} a -> s {maxResults = a} :: DescribeSpendLimits)

instance Core.AWSPager DescribeSpendLimits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpendLimitsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpendLimitsResponse_spendLimits
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSpendLimits_nextToken
          Lens..~ rs
          Lens.^? describeSpendLimitsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSpendLimits where
  type
    AWSResponse DescribeSpendLimits =
      DescribeSpendLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSpendLimitsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SpendLimits" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpendLimits where
  hashWithSalt _salt DescribeSpendLimits' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeSpendLimits where
  rnf DescribeSpendLimits' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeSpendLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeSpendLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSpendLimits where
  toJSON DescribeSpendLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeSpendLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSpendLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSpendLimitsResponse' smart constructor.
data DescribeSpendLimitsResponse = DescribeSpendLimitsResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of SpendLimit objects that contain the details for the
    -- requested spend limits.
    spendLimits :: Prelude.Maybe [SpendLimit],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpendLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpendLimitsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'spendLimits', 'describeSpendLimitsResponse_spendLimits' - An array of SpendLimit objects that contain the details for the
-- requested spend limits.
--
-- 'httpStatus', 'describeSpendLimitsResponse_httpStatus' - The response's http status code.
newDescribeSpendLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpendLimitsResponse
newDescribeSpendLimitsResponse pHttpStatus_ =
  DescribeSpendLimitsResponse'
    { nextToken =
        Prelude.Nothing,
      spendLimits = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeSpendLimitsResponse_nextToken :: Lens.Lens' DescribeSpendLimitsResponse (Prelude.Maybe Prelude.Text)
describeSpendLimitsResponse_nextToken = Lens.lens (\DescribeSpendLimitsResponse' {nextToken} -> nextToken) (\s@DescribeSpendLimitsResponse' {} a -> s {nextToken = a} :: DescribeSpendLimitsResponse)

-- | An array of SpendLimit objects that contain the details for the
-- requested spend limits.
describeSpendLimitsResponse_spendLimits :: Lens.Lens' DescribeSpendLimitsResponse (Prelude.Maybe [SpendLimit])
describeSpendLimitsResponse_spendLimits = Lens.lens (\DescribeSpendLimitsResponse' {spendLimits} -> spendLimits) (\s@DescribeSpendLimitsResponse' {} a -> s {spendLimits = a} :: DescribeSpendLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSpendLimitsResponse_httpStatus :: Lens.Lens' DescribeSpendLimitsResponse Prelude.Int
describeSpendLimitsResponse_httpStatus = Lens.lens (\DescribeSpendLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeSpendLimitsResponse' {} a -> s {httpStatus = a} :: DescribeSpendLimitsResponse)

instance Prelude.NFData DescribeSpendLimitsResponse where
  rnf DescribeSpendLimitsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spendLimits
      `Prelude.seq` Prelude.rnf httpStatus
