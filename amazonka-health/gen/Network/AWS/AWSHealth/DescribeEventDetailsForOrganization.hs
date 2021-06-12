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
-- Module      : Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events for one
-- or more accounts in your organization. Information includes standard
-- event data (AWS Region, service, and so on, as returned by
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization>),
-- a detailed event description, and possible additional metadata that
-- depends upon the nature of the event. Affected entities are not
-- included; to retrieve those, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
-- operation.
--
-- Before you can call this operation, you must first enable AWS Health to
-- work with AWS Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- When you call the @DescribeEventDetailsForOrganization@ operation, you
-- specify the @organizationEventDetailFilters@ object in the request.
-- Depending on the AWS Health event type, note the following differences:
--
-- -   If the event is public, the @awsAccountId@ parameter must be empty.
--     If you specify an account ID for a public event, then an error
--     message is returned. That\'s because the event might apply to all
--     AWS accounts and isn\'t specific to an account in your organization.
--
-- -   If the event is specific to an account, then you must specify the
--     @awsAccountId@ parameter in the request. If you don\'t specify an
--     account ID, an error message returns because the event is specific
--     to an AWS account in your organization.
--
-- For more information, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- This operation doesn\'t support resource-level permissions. You can\'t
-- use this operation to allow or deny access to specific AWS Health
-- events. For more information, see
-- <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
-- in the /AWS Health User Guide/.
module Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
  ( -- * Creating a Request
    DescribeEventDetailsForOrganization (..),
    newDescribeEventDetailsForOrganization,

    -- * Request Lenses
    describeEventDetailsForOrganization_locale,
    describeEventDetailsForOrganization_organizationEventDetailFilters,

    -- * Destructuring the Response
    DescribeEventDetailsForOrganizationResponse (..),
    newDescribeEventDetailsForOrganizationResponse,

    -- * Response Lenses
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventDetailsForOrganization' smart constructor.
data DescribeEventDetailsForOrganization = DescribeEventDetailsForOrganization'
  { -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Core.Maybe Core.Text,
    -- | A set of JSON elements that includes the @awsAccountId@ and the
    -- @eventArn@.
    organizationEventDetailFilters :: Core.NonEmpty EventAccountFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventDetailsForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'describeEventDetailsForOrganization_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'organizationEventDetailFilters', 'describeEventDetailsForOrganization_organizationEventDetailFilters' - A set of JSON elements that includes the @awsAccountId@ and the
-- @eventArn@.
newDescribeEventDetailsForOrganization ::
  -- | 'organizationEventDetailFilters'
  Core.NonEmpty EventAccountFilter ->
  DescribeEventDetailsForOrganization
newDescribeEventDetailsForOrganization
  pOrganizationEventDetailFilters_ =
    DescribeEventDetailsForOrganization'
      { locale =
          Core.Nothing,
        organizationEventDetailFilters =
          Lens._Coerce
            Lens.# pOrganizationEventDetailFilters_
      }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventDetailsForOrganization_locale :: Lens.Lens' DescribeEventDetailsForOrganization (Core.Maybe Core.Text)
describeEventDetailsForOrganization_locale = Lens.lens (\DescribeEventDetailsForOrganization' {locale} -> locale) (\s@DescribeEventDetailsForOrganization' {} a -> s {locale = a} :: DescribeEventDetailsForOrganization)

-- | A set of JSON elements that includes the @awsAccountId@ and the
-- @eventArn@.
describeEventDetailsForOrganization_organizationEventDetailFilters :: Lens.Lens' DescribeEventDetailsForOrganization (Core.NonEmpty EventAccountFilter)
describeEventDetailsForOrganization_organizationEventDetailFilters = Lens.lens (\DescribeEventDetailsForOrganization' {organizationEventDetailFilters} -> organizationEventDetailFilters) (\s@DescribeEventDetailsForOrganization' {} a -> s {organizationEventDetailFilters = a} :: DescribeEventDetailsForOrganization) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DescribeEventDetailsForOrganization
  where
  type
    AWSResponse DescribeEventDetailsForOrganization =
      DescribeEventDetailsForOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventDetailsForOrganizationResponse'
            Core.<$> (x Core..?> "successfulSet" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedSet" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeEventDetailsForOrganization

instance
  Core.NFData
    DescribeEventDetailsForOrganization

instance
  Core.ToHeaders
    DescribeEventDetailsForOrganization
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventDetailsForOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeEventDetailsForOrganization
  where
  toJSON DescribeEventDetailsForOrganization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("locale" Core..=) Core.<$> locale,
            Core.Just
              ( "organizationEventDetailFilters"
                  Core..= organizationEventDetailFilters
              )
          ]
      )

instance
  Core.ToPath
    DescribeEventDetailsForOrganization
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeEventDetailsForOrganization
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventDetailsForOrganizationResponse' smart constructor.
data DescribeEventDetailsForOrganizationResponse = DescribeEventDetailsForOrganizationResponse'
  { -- | Information about the events that could be retrieved.
    successfulSet :: Core.Maybe [OrganizationEventDetails],
    -- | Error messages for any events that could not be retrieved.
    failedSet :: Core.Maybe [OrganizationEventDetailsErrorItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventDetailsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulSet', 'describeEventDetailsForOrganizationResponse_successfulSet' - Information about the events that could be retrieved.
--
-- 'failedSet', 'describeEventDetailsForOrganizationResponse_failedSet' - Error messages for any events that could not be retrieved.
--
-- 'httpStatus', 'describeEventDetailsForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeEventDetailsForOrganizationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventDetailsForOrganizationResponse
newDescribeEventDetailsForOrganizationResponse
  pHttpStatus_ =
    DescribeEventDetailsForOrganizationResponse'
      { successfulSet =
          Core.Nothing,
        failedSet = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the events that could be retrieved.
describeEventDetailsForOrganizationResponse_successfulSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Core.Maybe [OrganizationEventDetails])
describeEventDetailsForOrganizationResponse_successfulSet = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {successfulSet} -> successfulSet) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {successfulSet = a} :: DescribeEventDetailsForOrganizationResponse) Core.. Lens.mapping Lens._Coerce

-- | Error messages for any events that could not be retrieved.
describeEventDetailsForOrganizationResponse_failedSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Core.Maybe [OrganizationEventDetailsErrorItem])
describeEventDetailsForOrganizationResponse_failedSet = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {failedSet} -> failedSet) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {failedSet = a} :: DescribeEventDetailsForOrganizationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventDetailsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeEventDetailsForOrganizationResponse Core.Int
describeEventDetailsForOrganizationResponse_httpStatus = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeEventDetailsForOrganizationResponse)

instance
  Core.NFData
    DescribeEventDetailsForOrganizationResponse
