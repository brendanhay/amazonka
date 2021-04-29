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
-- Module      : Network.AWS.SSM.DescribeActivations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about the activation, such as the date and time the
-- activation was created, its expiration date, the IAM role assigned to
-- the instances in the activation, and the number of instances registered
-- by using this activation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeActivations
  ( -- * Creating a Request
    DescribeActivations (..),
    newDescribeActivations,

    -- * Request Lenses
    describeActivations_nextToken,
    describeActivations_maxResults,
    describeActivations_filters,

    -- * Destructuring the Response
    DescribeActivationsResponse (..),
    newDescribeActivationsResponse,

    -- * Response Lenses
    describeActivationsResponse_nextToken,
    describeActivationsResponse_activationList,
    describeActivationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter to view information about your activations.
    filters :: Prelude.Maybe [DescribeActivationsFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeActivations_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeActivations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeActivations_filters' - A filter to view information about your activations.
newDescribeActivations ::
  DescribeActivations
newDescribeActivations =
  DescribeActivations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeActivations_nextToken :: Lens.Lens' DescribeActivations (Prelude.Maybe Prelude.Text)
describeActivations_nextToken = Lens.lens (\DescribeActivations' {nextToken} -> nextToken) (\s@DescribeActivations' {} a -> s {nextToken = a} :: DescribeActivations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeActivations_maxResults :: Lens.Lens' DescribeActivations (Prelude.Maybe Prelude.Natural)
describeActivations_maxResults = Lens.lens (\DescribeActivations' {maxResults} -> maxResults) (\s@DescribeActivations' {} a -> s {maxResults = a} :: DescribeActivations)

-- | A filter to view information about your activations.
describeActivations_filters :: Lens.Lens' DescribeActivations (Prelude.Maybe [DescribeActivationsFilter])
describeActivations_filters = Lens.lens (\DescribeActivations' {filters} -> filters) (\s@DescribeActivations' {} a -> s {filters = a} :: DescribeActivations) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeActivations where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeActivationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeActivationsResponse_activationList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeActivations_nextToken
          Lens..~ rs
          Lens.^? describeActivationsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeActivations where
  type
    Rs DescribeActivations =
      DescribeActivationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivationsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ActivationList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeActivations

instance Prelude.NFData DescribeActivations

instance Prelude.ToHeaders DescribeActivations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribeActivations" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeActivations where
  toJSON DescribeActivations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath DescribeActivations where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeActivations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of activations for your AWS account.
    activationList :: Prelude.Maybe [Activation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeActivationsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'activationList', 'describeActivationsResponse_activationList' - A list of activations for your AWS account.
--
-- 'httpStatus', 'describeActivationsResponse_httpStatus' - The response's http status code.
newDescribeActivationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActivationsResponse
newDescribeActivationsResponse pHttpStatus_ =
  DescribeActivationsResponse'
    { nextToken =
        Prelude.Nothing,
      activationList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeActivationsResponse_nextToken :: Lens.Lens' DescribeActivationsResponse (Prelude.Maybe Prelude.Text)
describeActivationsResponse_nextToken = Lens.lens (\DescribeActivationsResponse' {nextToken} -> nextToken) (\s@DescribeActivationsResponse' {} a -> s {nextToken = a} :: DescribeActivationsResponse)

-- | A list of activations for your AWS account.
describeActivationsResponse_activationList :: Lens.Lens' DescribeActivationsResponse (Prelude.Maybe [Activation])
describeActivationsResponse_activationList = Lens.lens (\DescribeActivationsResponse' {activationList} -> activationList) (\s@DescribeActivationsResponse' {} a -> s {activationList = a} :: DescribeActivationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeActivationsResponse_httpStatus :: Lens.Lens' DescribeActivationsResponse Prelude.Int
describeActivationsResponse_httpStatus = Lens.lens (\DescribeActivationsResponse' {httpStatus} -> httpStatus) (\s@DescribeActivationsResponse' {} a -> s {httpStatus = a} :: DescribeActivationsResponse)

instance Prelude.NFData DescribeActivationsResponse
