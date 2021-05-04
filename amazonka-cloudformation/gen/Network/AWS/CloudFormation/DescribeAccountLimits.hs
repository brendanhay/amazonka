{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFormation.DescribeAccountLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your account\'s AWS CloudFormation limits, such as the maximum
-- number of stacks that you can create in your account. For more
-- information about account limits, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits>
-- in the /AWS CloudFormation User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Request Lenses
    describeAccountLimits_nextToken,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeAccountLimits action.
--
-- /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { -- | A string that identifies the next page of limits that you want to
    -- retrieve.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager DescribeAccountLimits where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_accountLimits
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeAccountLimits_nextToken
          Lens..~ rs
          Lens.^? describeAccountLimitsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeAccountLimits where
  type
    Rs DescribeAccountLimits =
      DescribeAccountLimitsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> ( x Prelude..@? "AccountLimits"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits

instance Prelude.NFData DescribeAccountLimits

instance Prelude.ToHeaders DescribeAccountLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAccountLimits where
  toQuery DescribeAccountLimits' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeAccountLimits" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken
      ]

-- | The output for the DescribeAccountLimits action.
--
-- /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | An account limit structure that contain a list of AWS CloudFormation
    -- account limits and their values.
    accountLimits :: Prelude.Maybe [AccountLimit],
    -- | If the output exceeds 1 MB in size, a string that identifies the next
    -- page of limits. If no additional page exists, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountLimits', 'describeAccountLimitsResponse_accountLimits' - An account limit structure that contain a list of AWS CloudFormation
-- account limits and their values.
--
-- 'nextToken', 'describeAccountLimitsResponse_nextToken' - If the output exceeds 1 MB in size, a string that identifies the next
-- page of limits. If no additional page exists, this value is null.
--
-- 'httpStatus', 'describeAccountLimitsResponse_httpStatus' - The response's http status code.
newDescribeAccountLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountLimitsResponse
newDescribeAccountLimitsResponse pHttpStatus_ =
  DescribeAccountLimitsResponse'
    { accountLimits =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An account limit structure that contain a list of AWS CloudFormation
-- account limits and their values.
describeAccountLimitsResponse_accountLimits :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe [AccountLimit])
describeAccountLimitsResponse_accountLimits = Lens.lens (\DescribeAccountLimitsResponse' {accountLimits} -> accountLimits) (\s@DescribeAccountLimitsResponse' {} a -> s {accountLimits = a} :: DescribeAccountLimitsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If the output exceeds 1 MB in size, a string that identifies the next
-- page of limits. If no additional page exists, this value is null.
describeAccountLimitsResponse_nextToken :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Text)
describeAccountLimitsResponse_nextToken = Lens.lens (\DescribeAccountLimitsResponse' {nextToken} -> nextToken) (\s@DescribeAccountLimitsResponse' {} a -> s {nextToken = a} :: DescribeAccountLimitsResponse)

-- | The response's http status code.
describeAccountLimitsResponse_httpStatus :: Lens.Lens' DescribeAccountLimitsResponse Prelude.Int
describeAccountLimitsResponse_httpStatus = Lens.lens (\DescribeAccountLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountLimitsResponse' {} a -> s {httpStatus = a} :: DescribeAccountLimitsResponse)

instance Prelude.NFData DescribeAccountLimitsResponse
