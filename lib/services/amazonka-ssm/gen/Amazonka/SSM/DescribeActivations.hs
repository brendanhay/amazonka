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
-- Module      : Amazonka.SSM.DescribeActivations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about the activation, such as the date and time the
-- activation was created, its expiration date, the Identity and Access
-- Management (IAM) role assigned to the instances in the activation, and
-- the number of instances registered by using this activation.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeActivations
  ( -- * Creating a Request
    DescribeActivations (..),
    newDescribeActivations,

    -- * Request Lenses
    describeActivations_filters,
    describeActivations_nextToken,
    describeActivations_maxResults,

    -- * Destructuring the Response
    DescribeActivationsResponse (..),
    newDescribeActivationsResponse,

    -- * Response Lenses
    describeActivationsResponse_activationList,
    describeActivationsResponse_nextToken,
    describeActivationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { -- | A filter to view information about your activations.
    filters :: Prelude.Maybe [DescribeActivationsFilter],
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeActivations_filters' - A filter to view information about your activations.
--
-- 'nextToken', 'describeActivations_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeActivations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newDescribeActivations ::
  DescribeActivations
newDescribeActivations =
  DescribeActivations'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A filter to view information about your activations.
describeActivations_filters :: Lens.Lens' DescribeActivations (Prelude.Maybe [DescribeActivationsFilter])
describeActivations_filters = Lens.lens (\DescribeActivations' {filters} -> filters) (\s@DescribeActivations' {} a -> s {filters = a} :: DescribeActivations) Prelude.. Lens.mapping Lens.coerced

-- | A token to start the list. Use this token to get the next set of
-- results.
describeActivations_nextToken :: Lens.Lens' DescribeActivations (Prelude.Maybe Prelude.Text)
describeActivations_nextToken = Lens.lens (\DescribeActivations' {nextToken} -> nextToken) (\s@DescribeActivations' {} a -> s {nextToken = a} :: DescribeActivations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeActivations_maxResults :: Lens.Lens' DescribeActivations (Prelude.Maybe Prelude.Natural)
describeActivations_maxResults = Lens.lens (\DescribeActivations' {maxResults} -> maxResults) (\s@DescribeActivations' {} a -> s {maxResults = a} :: DescribeActivations)

instance Core.AWSPager DescribeActivations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeActivationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeActivationsResponse_activationList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeActivations_nextToken
          Lens..~ rs
          Lens.^? describeActivationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeActivations where
  type
    AWSResponse DescribeActivations =
      DescribeActivationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivationsResponse'
            Prelude.<$> (x Core..?> "ActivationList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeActivations where
  hashWithSalt salt' DescribeActivations' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribeActivations where
  rnf DescribeActivations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders DescribeActivations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeActivations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeActivations where
  toJSON DescribeActivations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeActivations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeActivations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { -- | A list of activations for your Amazon Web Services account.
    activationList :: Prelude.Maybe [Activation],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationList', 'describeActivationsResponse_activationList' - A list of activations for your Amazon Web Services account.
--
-- 'nextToken', 'describeActivationsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'describeActivationsResponse_httpStatus' - The response's http status code.
newDescribeActivationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActivationsResponse
newDescribeActivationsResponse pHttpStatus_ =
  DescribeActivationsResponse'
    { activationList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of activations for your Amazon Web Services account.
describeActivationsResponse_activationList :: Lens.Lens' DescribeActivationsResponse (Prelude.Maybe [Activation])
describeActivationsResponse_activationList = Lens.lens (\DescribeActivationsResponse' {activationList} -> activationList) (\s@DescribeActivationsResponse' {} a -> s {activationList = a} :: DescribeActivationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeActivationsResponse_nextToken :: Lens.Lens' DescribeActivationsResponse (Prelude.Maybe Prelude.Text)
describeActivationsResponse_nextToken = Lens.lens (\DescribeActivationsResponse' {nextToken} -> nextToken) (\s@DescribeActivationsResponse' {} a -> s {nextToken = a} :: DescribeActivationsResponse)

-- | The response's http status code.
describeActivationsResponse_httpStatus :: Lens.Lens' DescribeActivationsResponse Prelude.Int
describeActivationsResponse_httpStatus = Lens.lens (\DescribeActivationsResponse' {httpStatus} -> httpStatus) (\s@DescribeActivationsResponse' {} a -> s {httpStatus = a} :: DescribeActivationsResponse)

instance Prelude.NFData DescribeActivationsResponse where
  rnf DescribeActivationsResponse' {..} =
    Prelude.rnf activationList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
