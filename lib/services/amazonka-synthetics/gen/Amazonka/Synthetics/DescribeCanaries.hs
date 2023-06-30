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
-- Module      : Amazonka.Synthetics.DescribeCanaries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a list of the canaries in your account, along
-- with full details about each canary.
--
-- This operation supports resource-level authorization using an IAM policy
-- and the @Names@ parameter. If you specify the @Names@ parameter, the
-- operation is successful only if you have authorization to view all the
-- canaries that you specify in your request. If you do not have permission
-- to view any of the canaries, the request fails with a 403 response.
--
-- You are required to use the @Names@ parameter if you are logged on to a
-- user or role that has an IAM policy that restricts which canaries that
-- you are allowed to view. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Restricted.html Limiting a user to viewing specific canaries>.
module Amazonka.Synthetics.DescribeCanaries
  ( -- * Creating a Request
    DescribeCanaries (..),
    newDescribeCanaries,

    -- * Request Lenses
    describeCanaries_maxResults,
    describeCanaries_names,
    describeCanaries_nextToken,

    -- * Destructuring the Response
    DescribeCanariesResponse (..),
    newDescribeCanariesResponse,

    -- * Response Lenses
    describeCanariesResponse_canaries,
    describeCanariesResponse_nextToken,
    describeCanariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDescribeCanaries' smart constructor.
data DescribeCanaries = DescribeCanaries'
  { -- | Specify this parameter to limit how many canaries are returned each time
    -- you use the @DescribeCanaries@ operation. If you omit this parameter,
    -- the default of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter to return only canaries that match the names that you
    -- specify here. You can specify as many as five canary names.
    --
    -- If you specify this parameter, the operation is successful only if you
    -- have authorization to view all the canaries that you specify in your
    -- request. If you do not have permission to view any of the canaries, the
    -- request fails with a 403 response.
    --
    -- You are required to use this parameter if you are logged on to a user or
    -- role that has an IAM policy that restricts which canaries that you are
    -- allowed to view. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Restricted.html Limiting a user to viewing specific canaries>.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent operation to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeCanaries_maxResults' - Specify this parameter to limit how many canaries are returned each time
-- you use the @DescribeCanaries@ operation. If you omit this parameter,
-- the default of 100 is used.
--
-- 'names', 'describeCanaries_names' - Use this parameter to return only canaries that match the names that you
-- specify here. You can specify as many as five canary names.
--
-- If you specify this parameter, the operation is successful only if you
-- have authorization to view all the canaries that you specify in your
-- request. If you do not have permission to view any of the canaries, the
-- request fails with a 403 response.
--
-- You are required to use this parameter if you are logged on to a user or
-- role that has an IAM policy that restricts which canaries that you are
-- allowed to view. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Restricted.html Limiting a user to viewing specific canaries>.
--
-- 'nextToken', 'describeCanaries_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
newDescribeCanaries ::
  DescribeCanaries
newDescribeCanaries =
  DescribeCanaries'
    { maxResults = Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specify this parameter to limit how many canaries are returned each time
-- you use the @DescribeCanaries@ operation. If you omit this parameter,
-- the default of 100 is used.
describeCanaries_maxResults :: Lens.Lens' DescribeCanaries (Prelude.Maybe Prelude.Natural)
describeCanaries_maxResults = Lens.lens (\DescribeCanaries' {maxResults} -> maxResults) (\s@DescribeCanaries' {} a -> s {maxResults = a} :: DescribeCanaries)

-- | Use this parameter to return only canaries that match the names that you
-- specify here. You can specify as many as five canary names.
--
-- If you specify this parameter, the operation is successful only if you
-- have authorization to view all the canaries that you specify in your
-- request. If you do not have permission to view any of the canaries, the
-- request fails with a 403 response.
--
-- You are required to use this parameter if you are logged on to a user or
-- role that has an IAM policy that restricts which canaries that you are
-- allowed to view. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Restricted.html Limiting a user to viewing specific canaries>.
describeCanaries_names :: Lens.Lens' DescribeCanaries (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeCanaries_names = Lens.lens (\DescribeCanaries' {names} -> names) (\s@DescribeCanaries' {} a -> s {names = a} :: DescribeCanaries) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
describeCanaries_nextToken :: Lens.Lens' DescribeCanaries (Prelude.Maybe Prelude.Text)
describeCanaries_nextToken = Lens.lens (\DescribeCanaries' {nextToken} -> nextToken) (\s@DescribeCanaries' {} a -> s {nextToken = a} :: DescribeCanaries)

instance Core.AWSRequest DescribeCanaries where
  type
    AWSResponse DescribeCanaries =
      DescribeCanariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCanariesResponse'
            Prelude.<$> (x Data..?> "Canaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCanaries where
  hashWithSalt _salt DescribeCanaries' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeCanaries where
  rnf DescribeCanaries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeCanaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCanaries where
  toJSON DescribeCanaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("Names" Data..=) Prelude.<$> names,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeCanaries where
  toPath = Prelude.const "/canaries"

instance Data.ToQuery DescribeCanaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCanariesResponse' smart constructor.
data DescribeCanariesResponse = DescribeCanariesResponse'
  { -- | Returns an array. Each item in the array contains the full information
    -- about one canary.
    canaries :: Prelude.Maybe [Canary],
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeCanaries@ operation to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canaries', 'describeCanariesResponse_canaries' - Returns an array. Each item in the array contains the full information
-- about one canary.
--
-- 'nextToken', 'describeCanariesResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
--
-- 'httpStatus', 'describeCanariesResponse_httpStatus' - The response's http status code.
newDescribeCanariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCanariesResponse
newDescribeCanariesResponse pHttpStatus_ =
  DescribeCanariesResponse'
    { canaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns an array. Each item in the array contains the full information
-- about one canary.
describeCanariesResponse_canaries :: Lens.Lens' DescribeCanariesResponse (Prelude.Maybe [Canary])
describeCanariesResponse_canaries = Lens.lens (\DescribeCanariesResponse' {canaries} -> canaries) (\s@DescribeCanariesResponse' {} a -> s {canaries = a} :: DescribeCanariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
describeCanariesResponse_nextToken :: Lens.Lens' DescribeCanariesResponse (Prelude.Maybe Prelude.Text)
describeCanariesResponse_nextToken = Lens.lens (\DescribeCanariesResponse' {nextToken} -> nextToken) (\s@DescribeCanariesResponse' {} a -> s {nextToken = a} :: DescribeCanariesResponse)

-- | The response's http status code.
describeCanariesResponse_httpStatus :: Lens.Lens' DescribeCanariesResponse Prelude.Int
describeCanariesResponse_httpStatus = Lens.lens (\DescribeCanariesResponse' {httpStatus} -> httpStatus) (\s@DescribeCanariesResponse' {} a -> s {httpStatus = a} :: DescribeCanariesResponse)

instance Prelude.NFData DescribeCanariesResponse where
  rnf DescribeCanariesResponse' {..} =
    Prelude.rnf canaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
