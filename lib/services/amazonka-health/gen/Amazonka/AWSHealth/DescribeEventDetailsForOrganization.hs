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
-- Module      : Amazonka.AWSHealth.DescribeEventDetailsForOrganization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events for one
-- or more Amazon Web Services accounts in your organization. This
-- information includes standard event data (such as the Amazon Web
-- Services Region and service), an event description, and (depending on
-- the event) possible metadata. This operation doesn\'t return affected
-- entities, such as the resources related to the event. To return affected
-- entities, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
-- operation.
--
-- Before you can call this operation, you must first enable Health to work
-- with Organizations. To do this, call the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization>
-- operation from your organization\'s management account.
--
-- When you call the @DescribeEventDetailsForOrganization@ operation,
-- specify the @organizationEventDetailFilters@ object in the request.
-- Depending on the Health event type, note the following differences:
--
-- -   To return event details for a public event, you must specify a null
--     value for the @awsAccountId@ parameter. If you specify an account ID
--     for a public event, Health returns an error message because public
--     events aren\'t specific to an account.
--
-- -   To return event details for an event that is specific to an account
--     in your organization, you must specify the @awsAccountId@ parameter
--     in the request. If you don\'t specify an account ID, Health returns
--     an error message because the event is specific to an account in your
--     organization.
--
-- For more information, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>.
--
-- This operation doesn\'t support resource-level permissions. You can\'t
-- use this operation to allow or deny access to specific Health events.
-- For more information, see
-- <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
-- in the /Health User Guide/.
module Amazonka.AWSHealth.DescribeEventDetailsForOrganization
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
    describeEventDetailsForOrganizationResponse_failedSet,
    describeEventDetailsForOrganizationResponse_successfulSet,
    describeEventDetailsForOrganizationResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventDetailsForOrganization' smart constructor.
data DescribeEventDetailsForOrganization = DescribeEventDetailsForOrganization'
  { -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | A set of JSON elements that includes the @awsAccountId@ and the
    -- @eventArn@.
    organizationEventDetailFilters :: Prelude.NonEmpty EventAccountFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty EventAccountFilter ->
  DescribeEventDetailsForOrganization
newDescribeEventDetailsForOrganization
  pOrganizationEventDetailFilters_ =
    DescribeEventDetailsForOrganization'
      { locale =
          Prelude.Nothing,
        organizationEventDetailFilters =
          Lens.coerced
            Lens.# pOrganizationEventDetailFilters_
      }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventDetailsForOrganization_locale :: Lens.Lens' DescribeEventDetailsForOrganization (Prelude.Maybe Prelude.Text)
describeEventDetailsForOrganization_locale = Lens.lens (\DescribeEventDetailsForOrganization' {locale} -> locale) (\s@DescribeEventDetailsForOrganization' {} a -> s {locale = a} :: DescribeEventDetailsForOrganization)

-- | A set of JSON elements that includes the @awsAccountId@ and the
-- @eventArn@.
describeEventDetailsForOrganization_organizationEventDetailFilters :: Lens.Lens' DescribeEventDetailsForOrganization (Prelude.NonEmpty EventAccountFilter)
describeEventDetailsForOrganization_organizationEventDetailFilters = Lens.lens (\DescribeEventDetailsForOrganization' {organizationEventDetailFilters} -> organizationEventDetailFilters) (\s@DescribeEventDetailsForOrganization' {} a -> s {organizationEventDetailFilters = a} :: DescribeEventDetailsForOrganization) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DescribeEventDetailsForOrganization
  where
  type
    AWSResponse DescribeEventDetailsForOrganization =
      DescribeEventDetailsForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventDetailsForOrganizationResponse'
            Prelude.<$> (x Data..?> "failedSet" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "successfulSet" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEventDetailsForOrganization
  where
  hashWithSalt
    _salt
    DescribeEventDetailsForOrganization' {..} =
      _salt
        `Prelude.hashWithSalt` locale
        `Prelude.hashWithSalt` organizationEventDetailFilters

instance
  Prelude.NFData
    DescribeEventDetailsForOrganization
  where
  rnf DescribeEventDetailsForOrganization' {..} =
    Prelude.rnf locale `Prelude.seq`
      Prelude.rnf organizationEventDetailFilters

instance
  Data.ToHeaders
    DescribeEventDetailsForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHealth_20160804.DescribeEventDetailsForOrganization" ::
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
    DescribeEventDetailsForOrganization
  where
  toJSON DescribeEventDetailsForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            Prelude.Just
              ( "organizationEventDetailFilters"
                  Data..= organizationEventDetailFilters
              )
          ]
      )

instance
  Data.ToPath
    DescribeEventDetailsForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeEventDetailsForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventDetailsForOrganizationResponse' smart constructor.
data DescribeEventDetailsForOrganizationResponse = DescribeEventDetailsForOrganizationResponse'
  { -- | Error messages for any events that could not be retrieved.
    failedSet :: Prelude.Maybe [OrganizationEventDetailsErrorItem],
    -- | Information about the events that could be retrieved.
    successfulSet :: Prelude.Maybe [OrganizationEventDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventDetailsForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedSet', 'describeEventDetailsForOrganizationResponse_failedSet' - Error messages for any events that could not be retrieved.
--
-- 'successfulSet', 'describeEventDetailsForOrganizationResponse_successfulSet' - Information about the events that could be retrieved.
--
-- 'httpStatus', 'describeEventDetailsForOrganizationResponse_httpStatus' - The response's http status code.
newDescribeEventDetailsForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventDetailsForOrganizationResponse
newDescribeEventDetailsForOrganizationResponse
  pHttpStatus_ =
    DescribeEventDetailsForOrganizationResponse'
      { failedSet =
          Prelude.Nothing,
        successfulSet =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Error messages for any events that could not be retrieved.
describeEventDetailsForOrganizationResponse_failedSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Prelude.Maybe [OrganizationEventDetailsErrorItem])
describeEventDetailsForOrganizationResponse_failedSet = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {failedSet} -> failedSet) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {failedSet = a} :: DescribeEventDetailsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the events that could be retrieved.
describeEventDetailsForOrganizationResponse_successfulSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Prelude.Maybe [OrganizationEventDetails])
describeEventDetailsForOrganizationResponse_successfulSet = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {successfulSet} -> successfulSet) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {successfulSet = a} :: DescribeEventDetailsForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventDetailsForOrganizationResponse_httpStatus :: Lens.Lens' DescribeEventDetailsForOrganizationResponse Prelude.Int
describeEventDetailsForOrganizationResponse_httpStatus = Lens.lens (\DescribeEventDetailsForOrganizationResponse' {httpStatus} -> httpStatus) (\s@DescribeEventDetailsForOrganizationResponse' {} a -> s {httpStatus = a} :: DescribeEventDetailsForOrganizationResponse)

instance
  Prelude.NFData
    DescribeEventDetailsForOrganizationResponse
  where
  rnf DescribeEventDetailsForOrganizationResponse' {..} =
    Prelude.rnf failedSet `Prelude.seq`
      Prelude.rnf successfulSet `Prelude.seq`
        Prelude.rnf httpStatus
