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
-- Module      : Amazonka.CloudFormation.DescribeStackResourceDrifts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns drift information for the resources that have been checked for
-- drift in the specified stack. This includes actual and expected
-- configuration values for resources where CloudFormation detects
-- configuration drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack
-- resource that has been checked for drift. Resources that haven\'t yet
-- been checked for drift aren\'t included. Resources that don\'t currently
-- support drift detection aren\'t checked, and so not included. For a list
-- of resources that support drift detection, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- Use DetectStackResourceDrift to detect drift on individual resources, or
-- DetectStackDrift to detect drift on all supported resources for a given
-- stack.
module Amazonka.CloudFormation.DescribeStackResourceDrifts
  ( -- * Creating a Request
    DescribeStackResourceDrifts (..),
    newDescribeStackResourceDrifts,

    -- * Request Lenses
    describeStackResourceDrifts_stackResourceDriftStatusFilters,
    describeStackResourceDrifts_nextToken,
    describeStackResourceDrifts_maxResults,
    describeStackResourceDrifts_stackName,

    -- * Destructuring the Response
    DescribeStackResourceDriftsResponse (..),
    newDescribeStackResourceDriftsResponse,

    -- * Response Lenses
    describeStackResourceDriftsResponse_nextToken,
    describeStackResourceDriftsResponse_httpStatus,
    describeStackResourceDriftsResponse_stackResourceDrifts,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStackResourceDrifts' smart constructor.
data DescribeStackResourceDrifts = DescribeStackResourceDrifts'
  { -- | The resource drift status values to use as filters for the resource
    -- drift results returned.
    --
    -- -   @DELETED@: The resource differs from its expected template
    --     configuration in that the resource has been deleted.
    --
    -- -   @MODIFIED@: One or more resource properties differ from their
    --     expected template values.
    --
    -- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
    --     template configuration.
    --
    -- -   @NOT_CHECKED@: CloudFormation doesn\'t currently return this value.
    stackResourceDriftStatusFilters :: Prelude.Maybe (Prelude.NonEmpty StackResourceDriftStatus),
    -- | A string that identifies the next page of stack resource drift results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stack for which you want drift information.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackResourceDrifts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackResourceDriftStatusFilters', 'describeStackResourceDrifts_stackResourceDriftStatusFilters' - The resource drift status values to use as filters for the resource
-- drift results returned.
--
-- -   @DELETED@: The resource differs from its expected template
--     configuration in that the resource has been deleted.
--
-- -   @MODIFIED@: One or more resource properties differ from their
--     expected template values.
--
-- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
--     template configuration.
--
-- -   @NOT_CHECKED@: CloudFormation doesn\'t currently return this value.
--
-- 'nextToken', 'describeStackResourceDrifts_nextToken' - A string that identifies the next page of stack resource drift results.
--
-- 'maxResults', 'describeStackResourceDrifts_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'stackName', 'describeStackResourceDrifts_stackName' - The name of the stack for which you want drift information.
newDescribeStackResourceDrifts ::
  -- | 'stackName'
  Prelude.Text ->
  DescribeStackResourceDrifts
newDescribeStackResourceDrifts pStackName_ =
  DescribeStackResourceDrifts'
    { stackResourceDriftStatusFilters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      stackName = pStackName_
    }

-- | The resource drift status values to use as filters for the resource
-- drift results returned.
--
-- -   @DELETED@: The resource differs from its expected template
--     configuration in that the resource has been deleted.
--
-- -   @MODIFIED@: One or more resource properties differ from their
--     expected template values.
--
-- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
--     template configuration.
--
-- -   @NOT_CHECKED@: CloudFormation doesn\'t currently return this value.
describeStackResourceDrifts_stackResourceDriftStatusFilters :: Lens.Lens' DescribeStackResourceDrifts (Prelude.Maybe (Prelude.NonEmpty StackResourceDriftStatus))
describeStackResourceDrifts_stackResourceDriftStatusFilters = Lens.lens (\DescribeStackResourceDrifts' {stackResourceDriftStatusFilters} -> stackResourceDriftStatusFilters) (\s@DescribeStackResourceDrifts' {} a -> s {stackResourceDriftStatusFilters = a} :: DescribeStackResourceDrifts) Prelude.. Lens.mapping Lens.coerced

-- | A string that identifies the next page of stack resource drift results.
describeStackResourceDrifts_nextToken :: Lens.Lens' DescribeStackResourceDrifts (Prelude.Maybe Prelude.Text)
describeStackResourceDrifts_nextToken = Lens.lens (\DescribeStackResourceDrifts' {nextToken} -> nextToken) (\s@DescribeStackResourceDrifts' {} a -> s {nextToken = a} :: DescribeStackResourceDrifts)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
describeStackResourceDrifts_maxResults :: Lens.Lens' DescribeStackResourceDrifts (Prelude.Maybe Prelude.Natural)
describeStackResourceDrifts_maxResults = Lens.lens (\DescribeStackResourceDrifts' {maxResults} -> maxResults) (\s@DescribeStackResourceDrifts' {} a -> s {maxResults = a} :: DescribeStackResourceDrifts)

-- | The name of the stack for which you want drift information.
describeStackResourceDrifts_stackName :: Lens.Lens' DescribeStackResourceDrifts Prelude.Text
describeStackResourceDrifts_stackName = Lens.lens (\DescribeStackResourceDrifts' {stackName} -> stackName) (\s@DescribeStackResourceDrifts' {} a -> s {stackName = a} :: DescribeStackResourceDrifts)

instance Core.AWSRequest DescribeStackResourceDrifts where
  type
    AWSResponse DescribeStackResourceDrifts =
      DescribeStackResourceDriftsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStackResourceDriftsResult"
      ( \s h x ->
          DescribeStackResourceDriftsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "StackResourceDrifts"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeStackResourceDrifts where
  hashWithSalt _salt DescribeStackResourceDrifts' {..} =
    _salt
      `Prelude.hashWithSalt` stackResourceDriftStatusFilters
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData DescribeStackResourceDrifts where
  rnf DescribeStackResourceDrifts' {..} =
    Prelude.rnf stackResourceDriftStatusFilters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders DescribeStackResourceDrifts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStackResourceDrifts where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStackResourceDrifts where
  toQuery DescribeStackResourceDrifts' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeStackResourceDrifts" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackResourceDriftStatusFilters"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> stackResourceDriftStatusFilters
            ),
        "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults,
        "StackName" Data.=: stackName
      ]

-- | /See:/ 'newDescribeStackResourceDriftsResponse' smart constructor.
data DescribeStackResourceDriftsResponse = DescribeStackResourceDriftsResponse'
  { -- | If the request doesn\'t return all the remaining results, @NextToken@ is
    -- set to a token. To retrieve the next set of results, call
    -- @DescribeStackResourceDrifts@ again and assign that token to the request
    -- object\'s @NextToken@ parameter. If the request returns all results,
    -- @NextToken@ is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Drift information for the resources that have been checked for drift in
    -- the specified stack. This includes actual and expected configuration
    -- values for resources where CloudFormation detects drift.
    --
    -- For a given stack, there will be one @StackResourceDrift@ for each stack
    -- resource that has been checked for drift. Resources that haven\'t yet
    -- been checked for drift aren\'t included. Resources that do not currently
    -- support drift detection aren\'t checked, and so not included. For a list
    -- of resources that support drift detection, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
    stackResourceDrifts :: [StackResourceDrift]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackResourceDriftsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStackResourceDriftsResponse_nextToken' - If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call
-- @DescribeStackResourceDrifts@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If the request returns all results,
-- @NextToken@ is set to @null@.
--
-- 'httpStatus', 'describeStackResourceDriftsResponse_httpStatus' - The response's http status code.
--
-- 'stackResourceDrifts', 'describeStackResourceDriftsResponse_stackResourceDrifts' - Drift information for the resources that have been checked for drift in
-- the specified stack. This includes actual and expected configuration
-- values for resources where CloudFormation detects drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack
-- resource that has been checked for drift. Resources that haven\'t yet
-- been checked for drift aren\'t included. Resources that do not currently
-- support drift detection aren\'t checked, and so not included. For a list
-- of resources that support drift detection, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
newDescribeStackResourceDriftsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackResourceDriftsResponse
newDescribeStackResourceDriftsResponse pHttpStatus_ =
  DescribeStackResourceDriftsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stackResourceDrifts = Prelude.mempty
    }

-- | If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call
-- @DescribeStackResourceDrifts@ again and assign that token to the request
-- object\'s @NextToken@ parameter. If the request returns all results,
-- @NextToken@ is set to @null@.
describeStackResourceDriftsResponse_nextToken :: Lens.Lens' DescribeStackResourceDriftsResponse (Prelude.Maybe Prelude.Text)
describeStackResourceDriftsResponse_nextToken = Lens.lens (\DescribeStackResourceDriftsResponse' {nextToken} -> nextToken) (\s@DescribeStackResourceDriftsResponse' {} a -> s {nextToken = a} :: DescribeStackResourceDriftsResponse)

-- | The response's http status code.
describeStackResourceDriftsResponse_httpStatus :: Lens.Lens' DescribeStackResourceDriftsResponse Prelude.Int
describeStackResourceDriftsResponse_httpStatus = Lens.lens (\DescribeStackResourceDriftsResponse' {httpStatus} -> httpStatus) (\s@DescribeStackResourceDriftsResponse' {} a -> s {httpStatus = a} :: DescribeStackResourceDriftsResponse)

-- | Drift information for the resources that have been checked for drift in
-- the specified stack. This includes actual and expected configuration
-- values for resources where CloudFormation detects drift.
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack
-- resource that has been checked for drift. Resources that haven\'t yet
-- been checked for drift aren\'t included. Resources that do not currently
-- support drift detection aren\'t checked, and so not included. For a list
-- of resources that support drift detection, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
describeStackResourceDriftsResponse_stackResourceDrifts :: Lens.Lens' DescribeStackResourceDriftsResponse [StackResourceDrift]
describeStackResourceDriftsResponse_stackResourceDrifts = Lens.lens (\DescribeStackResourceDriftsResponse' {stackResourceDrifts} -> stackResourceDrifts) (\s@DescribeStackResourceDriftsResponse' {} a -> s {stackResourceDrifts = a} :: DescribeStackResourceDriftsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeStackResourceDriftsResponse
  where
  rnf DescribeStackResourceDriftsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stackResourceDrifts
