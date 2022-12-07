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
-- Module      : Amazonka.CloudFormation.DescribeAccountLimits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your account\'s CloudFormation limits, such as the maximum
-- number of stacks that you can create in your account. For more
-- information about account limits, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html CloudFormation Quotas>
-- in the /CloudFormation User Guide/.
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Request Lenses
    describeAccountLimits_nextToken,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DescribeAccountLimits action.
--
-- /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { -- | A string that identifies the next page of limits that you want to
    -- retrieve.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'describeAccountLimits_nextToken' - A string that identifies the next page of limits that you want to
-- retrieve.
newDescribeAccountLimits ::
  DescribeAccountLimits
newDescribeAccountLimits =
  DescribeAccountLimits' {nextToken = Prelude.Nothing}

-- | A string that identifies the next page of limits that you want to
-- retrieve.
describeAccountLimits_nextToken :: Lens.Lens' DescribeAccountLimits (Prelude.Maybe Prelude.Text)
describeAccountLimits_nextToken = Lens.lens (\DescribeAccountLimits' {nextToken} -> nextToken) (\s@DescribeAccountLimits' {} a -> s {nextToken = a} :: DescribeAccountLimits)

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
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "AccountLimits" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits where
  hashWithSalt _salt DescribeAccountLimits' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAccountLimits where
  rnf DescribeAccountLimits' {..} =
    Prelude.rnf nextToken

instance Data.ToHeaders DescribeAccountLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountLimits where
  toQuery DescribeAccountLimits' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAccountLimits" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken
      ]

-- | The output for the DescribeAccountLimits action.
--
-- /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next
    -- page of limits. If no additional page exists, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An account limit structure that contain a list of CloudFormation account
    -- limits and their values.
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
-- 'nextToken', 'describeAccountLimitsResponse_nextToken' - If the output exceeds 1 MB in size, a string that identifies the next
-- page of limits. If no additional page exists, this value is null.
--
-- 'accountLimits', 'describeAccountLimitsResponse_accountLimits' - An account limit structure that contain a list of CloudFormation account
-- limits and their values.
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

-- | If the output exceeds 1 MB in size, a string that identifies the next
-- page of limits. If no additional page exists, this value is null.
describeAccountLimitsResponse_nextToken :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Text)
describeAccountLimitsResponse_nextToken = Lens.lens (\DescribeAccountLimitsResponse' {nextToken} -> nextToken) (\s@DescribeAccountLimitsResponse' {} a -> s {nextToken = a} :: DescribeAccountLimitsResponse)

-- | An account limit structure that contain a list of CloudFormation account
-- limits and their values.
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
