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
-- Module      : Amazonka.AWSHealth.DescribeEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events.
-- Information includes standard event data (Amazon Web Services Region,
-- service, and so on, as returned by
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents>),
-- a detailed event description, and possible additional metadata that
-- depends upon the nature of the event. Affected entities are not
-- included. To retrieve the entities, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities>
-- operation.
--
-- If a specified event can\'t be retrieved, an error message is returned
-- for that event.
--
-- This operation supports resource-level permissions. You can use this
-- operation to allow or deny access to specific Health events. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
-- in the /Health User Guide/.
module Amazonka.AWSHealth.DescribeEventDetails
  ( -- * Creating a Request
    DescribeEventDetails (..),
    newDescribeEventDetails,

    -- * Request Lenses
    describeEventDetails_locale,
    describeEventDetails_eventArns,

    -- * Destructuring the Response
    DescribeEventDetailsResponse (..),
    newDescribeEventDetailsResponse,

    -- * Response Lenses
    describeEventDetailsResponse_successfulSet,
    describeEventDetailsResponse_failedSet,
    describeEventDetailsResponse_httpStatus,
  )
where

import Amazonka.AWSHealth.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventDetails' smart constructor.
data DescribeEventDetails = DescribeEventDetails'
  { -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Prelude.Maybe Prelude.Text,
    -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'describeEventDetails_locale' - The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
--
-- 'eventArns', 'describeEventDetails_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
newDescribeEventDetails ::
  -- | 'eventArns'
  Prelude.NonEmpty Prelude.Text ->
  DescribeEventDetails
newDescribeEventDetails pEventArns_ =
  DescribeEventDetails'
    { locale = Prelude.Nothing,
      eventArns = Lens.coerced Lens.# pEventArns_
    }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventDetails_locale :: Lens.Lens' DescribeEventDetails (Prelude.Maybe Prelude.Text)
describeEventDetails_locale = Lens.lens (\DescribeEventDetails' {locale} -> locale) (\s@DescribeEventDetails' {} a -> s {locale = a} :: DescribeEventDetails)

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
describeEventDetails_eventArns :: Lens.Lens' DescribeEventDetails (Prelude.NonEmpty Prelude.Text)
describeEventDetails_eventArns = Lens.lens (\DescribeEventDetails' {eventArns} -> eventArns) (\s@DescribeEventDetails' {} a -> s {eventArns = a} :: DescribeEventDetails) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeEventDetails where
  type
    AWSResponse DescribeEventDetails =
      DescribeEventDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventDetailsResponse'
            Prelude.<$> (x Core..?> "successfulSet" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failedSet" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventDetails where
  hashWithSalt _salt DescribeEventDetails' {..} =
    _salt `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` eventArns

instance Prelude.NFData DescribeEventDetails where
  rnf DescribeEventDetails' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf eventArns

instance Core.ToHeaders DescribeEventDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEventDetails where
  toJSON DescribeEventDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("locale" Core..=) Prelude.<$> locale,
            Prelude.Just ("eventArns" Core..= eventArns)
          ]
      )

instance Core.ToPath DescribeEventDetails where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEventDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventDetailsResponse' smart constructor.
data DescribeEventDetailsResponse = DescribeEventDetailsResponse'
  { -- | Information about the events that could be retrieved.
    successfulSet :: Prelude.Maybe [EventDetails],
    -- | Error messages for any events that could not be retrieved.
    failedSet :: Prelude.Maybe [EventDetailsErrorItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulSet', 'describeEventDetailsResponse_successfulSet' - Information about the events that could be retrieved.
--
-- 'failedSet', 'describeEventDetailsResponse_failedSet' - Error messages for any events that could not be retrieved.
--
-- 'httpStatus', 'describeEventDetailsResponse_httpStatus' - The response's http status code.
newDescribeEventDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventDetailsResponse
newDescribeEventDetailsResponse pHttpStatus_ =
  DescribeEventDetailsResponse'
    { successfulSet =
        Prelude.Nothing,
      failedSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the events that could be retrieved.
describeEventDetailsResponse_successfulSet :: Lens.Lens' DescribeEventDetailsResponse (Prelude.Maybe [EventDetails])
describeEventDetailsResponse_successfulSet = Lens.lens (\DescribeEventDetailsResponse' {successfulSet} -> successfulSet) (\s@DescribeEventDetailsResponse' {} a -> s {successfulSet = a} :: DescribeEventDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Error messages for any events that could not be retrieved.
describeEventDetailsResponse_failedSet :: Lens.Lens' DescribeEventDetailsResponse (Prelude.Maybe [EventDetailsErrorItem])
describeEventDetailsResponse_failedSet = Lens.lens (\DescribeEventDetailsResponse' {failedSet} -> failedSet) (\s@DescribeEventDetailsResponse' {} a -> s {failedSet = a} :: DescribeEventDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventDetailsResponse_httpStatus :: Lens.Lens' DescribeEventDetailsResponse Prelude.Int
describeEventDetailsResponse_httpStatus = Lens.lens (\DescribeEventDetailsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventDetailsResponse' {} a -> s {httpStatus = a} :: DescribeEventDetailsResponse)

instance Prelude.NFData DescribeEventDetailsResponse where
  rnf DescribeEventDetailsResponse' {..} =
    Prelude.rnf successfulSet
      `Prelude.seq` Prelude.rnf failedSet
      `Prelude.seq` Prelude.rnf httpStatus
