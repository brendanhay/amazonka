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
-- Module      : Amazonka.AWSHealth.DescribeAffectedAccountsForOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of accounts in the organization from Organizations that
-- are affected by the provided event. For more information about the
-- different types of Health events, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- Before you can call this operation, you must first enable Health to work
-- with Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the next request to return more results.
--
-- This operation returns paginated results.
module Amazonka.AWSHealth.DescribeAffectedAccountsForOrganization
  ( -- * Creating a Request
    DescribeAffectedAccountsForOrganization (..),
    newDescribeAffectedAccountsForOrganization,

    -- * Request Lenses
    describeAffectedAccountsForOrganization_maxResults,
    describeAffectedAccountsForOrganization_nextToken,
    describeAffectedAccountsForOrganization_eventArn,

    -- * Destructuring the Response
    DescribeAffectedAccountsForOrganizationResponse (..),
    newDescribeAffectedAccountsForOrganizationResponse,

    -- * Response Lenses
    describeAffectedAccountsForOrganizationResponse_affectedAccounts,
    describeAffectedAccountsForOrganizationResponse_eventScopeCode,
    describeAffectedAccountsForOrganizationResponse_nextToken,
    describeAffectedAccountsForOrganizationResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAffectedAccountsForOrganization' smart constructor.
data DescribeAffectedAccountsForOrganization = DescribeAffectedAccountsForOrganization'
  { -- | The maximum number of items to return in one batch, between 10 and 100,
    -- inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedAccountsForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeAffectedAccountsForOrganization_maxResults' - The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
--
-- 'nextToken', 'describeAffectedAccountsForOrganization_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'eventArn', 'describeAffectedAccountsForOrganization_eventArn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
newDescribeAffectedAccountsForOrganization ::
  -- | 'eventArn'
  Prelude.Text ->
  DescribeAffectedAccountsForOrganization
newDescribeAffectedAccountsForOrganization pEventArn_ =
  DescribeAffectedAccountsForOrganization'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      eventArn = pEventArn_
    }

-- | The maximum number of items to return in one batch, between 10 and 100,
-- inclusive.
describeAffectedAccountsForOrganization_maxResults :: Lens.Lens' DescribeAffectedAccountsForOrganization (Prelude.Maybe Prelude.Natural)
describeAffectedAccountsForOrganization_maxResults = Lens.lens (\DescribeAffectedAccountsForOrganization' {maxResults} -> maxResults) (\s@DescribeAffectedAccountsForOrganization' {} a -> s {maxResults = a} :: DescribeAffectedAccountsForOrganization)

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedAccountsForOrganization_nextToken :: Lens.Lens' DescribeAffectedAccountsForOrganization (Prelude.Maybe Prelude.Text)
describeAffectedAccountsForOrganization_nextToken = Lens.lens (\DescribeAffectedAccountsForOrganization' {nextToken} -> nextToken) (\s@DescribeAffectedAccountsForOrganization' {} a -> s {nextToken = a} :: DescribeAffectedAccountsForOrganization)

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
describeAffectedAccountsForOrganization_eventArn :: Lens.Lens' DescribeAffectedAccountsForOrganization Prelude.Text
describeAffectedAccountsForOrganization_eventArn = Lens.lens (\DescribeAffectedAccountsForOrganization' {eventArn} -> eventArn) (\s@DescribeAffectedAccountsForOrganization' {} a -> s {eventArn = a} :: DescribeAffectedAccountsForOrganization)

instance
  Core.AWSPager
    DescribeAffectedAccountsForOrganization
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAffectedAccountsForOrganizationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAffectedAccountsForOrganizationResponse_affectedAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeAffectedAccountsForOrganization_nextToken
          Lens..~ rs
          Lens.^? describeAffectedAccountsForOrganizationResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAffectedAccountsForOrganization
  where
  type
    AWSResponse
      DescribeAffectedAccountsForOrganization =
      DescribeAffectedAccountsForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedAccountsForOrganizationResponse'
            Prelude.<$> ( x
                            Data..?> "affectedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "eventScopeCode")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAffectedAccountsForOrganization
  where
  hashWithSalt
    _salt
    DescribeAffectedAccountsForOrganization' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` eventArn

instance
  Prelude.NFData
    DescribeAffectedAccountsForOrganization
  where
  rnf DescribeAffectedAccountsForOrganization' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventArn

instance
  Data.ToHeaders
    DescribeAffectedAccountsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeAffectedAccountsForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeAffectedAccountsForOrganization
  where
  toJSON DescribeAffectedAccountsForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("eventArn" Data..= eventArn)
          ]
      )

instance
  Data.ToPath
    DescribeAffectedAccountsForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAffectedAccountsForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAffectedAccountsForOrganizationResponse' smart constructor.
data DescribeAffectedAccountsForOrganizationResponse = DescribeAffectedAccountsForOrganizationResponse'
  { -- | A JSON set of elements of the affected accounts.
    affectedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | This parameter specifies if the Health event is a public Amazon Web
    -- Services service event or an account-specific event.
    --
    -- -   If the @eventScopeCode@ value is @PUBLIC@, then the
    --     @affectedAccounts@ value is always empty.
    --
    -- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
    --     @affectedAccounts@ value lists the affected Amazon Web Services
    --     accounts in your organization. For example, if an event affects a
    --     service such as Amazon Elastic Compute Cloud and you have Amazon Web
    --     Services accounts that use that service, those account IDs appear in
    --     the response.
    --
    -- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
    --     you specified in the request is invalid or doesn\'t exist.
    eventScopeCode :: Prelude.Maybe EventScopeCode,
    -- | If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next batch of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAffectedAccountsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedAccounts', 'describeAffectedAccountsForOrganizationResponse_affectedAccounts' - A JSON set of elements of the affected accounts.
--
-- 'eventScopeCode', 'describeAffectedAccountsForOrganizationResponse_eventScopeCode' - This parameter specifies if the Health event is a public Amazon Web
-- Services service event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected Amazon Web Services
--     accounts in your organization. For example, if an event affects a
--     service such as Amazon Elastic Compute Cloud and you have Amazon Web
--     Services accounts that use that service, those account IDs appear in
--     the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
--
-- 'nextToken', 'describeAffectedAccountsForOrganizationResponse_nextToken' - If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'httpStatus', 'describeAffectedAccountsForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeAffectedAccountsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAffectedAccountsForOrganizationResponse
newDescribeAffectedAccountsForOrganizationResponse
  pHttpStatus_ =
    DescribeAffectedAccountsForOrganizationResponse'
      { affectedAccounts =
          Prelude.Nothing,
        eventScopeCode =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A JSON set of elements of the affected accounts.
describeAffectedAccountsForOrganizationResponse_affectedAccounts :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Prelude.Maybe [Prelude.Text])
describeAffectedAccountsForOrganizationResponse_affectedAccounts = Lens.lens (\DescribeAffectedAccountsForOrganizationResponse' {affectedAccounts} -> affectedAccounts) (\s@DescribeAffectedAccountsForOrganizationResponse' {} a -> s {affectedAccounts = a} :: DescribeAffectedAccountsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | This parameter specifies if the Health event is a public Amazon Web
-- Services service event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected Amazon Web Services
--     accounts in your organization. For example, if an event affects a
--     service such as Amazon Elastic Compute Cloud and you have Amazon Web
--     Services accounts that use that service, those account IDs appear in
--     the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
describeAffectedAccountsForOrganizationResponse_eventScopeCode :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Prelude.Maybe EventScopeCode)
describeAffectedAccountsForOrganizationResponse_eventScopeCode = Lens.lens (\DescribeAffectedAccountsForOrganizationResponse' {eventScopeCode} -> eventScopeCode) (\s@DescribeAffectedAccountsForOrganizationResponse' {} a -> s {eventScopeCode = a} :: DescribeAffectedAccountsForOrganizationResponse)

-- | If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next batch of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
describeAffectedAccountsForOrganizationResponse_nextToken :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Prelude.Maybe Prelude.Text)
describeAffectedAccountsForOrganizationResponse_nextToken = Lens.lens (\DescribeAffectedAccountsForOrganizationResponse' {nextToken} -> nextToken) (\s@DescribeAffectedAccountsForOrganizationResponse' {} a -> s {nextToken = a} :: DescribeAffectedAccountsForOrganizationResponse)

-- | The response's http status code.
describeAffectedAccountsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse Prelude.Int
describeAffectedAccountsForOrganizationResponse_httpStatus = Lens.lens (\DescribeAffectedAccountsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeAffectedAccountsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeAffectedAccountsForOrganizationResponse)

instance
  Prelude.NFData
    DescribeAffectedAccountsForOrganizationResponse
  where
  rnf
    DescribeAffectedAccountsForOrganizationResponse' {..} =
      Prelude.rnf affectedAccounts
        `Prelude.seq` Prelude.rnf eventScopeCode
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
