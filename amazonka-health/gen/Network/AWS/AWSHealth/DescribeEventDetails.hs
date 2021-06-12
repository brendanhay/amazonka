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
-- Module      : Network.AWS.AWSHealth.DescribeEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events.
-- Information includes standard event data (AWS Region, service, and so
-- on, as returned by
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents>),
-- a detailed event description, and possible additional metadata that
-- depends upon the nature of the event. Affected entities are not
-- included. To retrieve those, use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities>
-- operation.
--
-- If a specified event cannot be retrieved, an error message is returned
-- for that event.
--
-- This operation supports resource-level permissions. You can use this
-- operation to allow or deny access to specific AWS Health events. For
-- more information, see
-- <https://docs.aws.amazon.com/health/latest/ug/security_iam_id-based-policy-examples.html#resource-action-based-conditions Resource- and action-based conditions>
-- in the /AWS Health User Guide/.
module Network.AWS.AWSHealth.DescribeEventDetails
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

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventDetails' smart constructor.
data DescribeEventDetails = DescribeEventDetails'
  { -- | The locale (language) to return information in. English (en) is the
    -- default and the only supported value at this time.
    locale :: Core.Maybe Core.Text,
    -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeEventDetails
newDescribeEventDetails pEventArns_ =
  DescribeEventDetails'
    { locale = Core.Nothing,
      eventArns = Lens._Coerce Lens.# pEventArns_
    }

-- | The locale (language) to return information in. English (en) is the
-- default and the only supported value at this time.
describeEventDetails_locale :: Lens.Lens' DescribeEventDetails (Core.Maybe Core.Text)
describeEventDetails_locale = Lens.lens (\DescribeEventDetails' {locale} -> locale) (\s@DescribeEventDetails' {} a -> s {locale = a} :: DescribeEventDetails)

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
describeEventDetails_eventArns :: Lens.Lens' DescribeEventDetails (Core.NonEmpty Core.Text)
describeEventDetails_eventArns = Lens.lens (\DescribeEventDetails' {eventArns} -> eventArns) (\s@DescribeEventDetails' {} a -> s {eventArns = a} :: DescribeEventDetails) Core.. Lens._Coerce

instance Core.AWSRequest DescribeEventDetails where
  type
    AWSResponse DescribeEventDetails =
      DescribeEventDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventDetailsResponse'
            Core.<$> (x Core..?> "successfulSet" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedSet" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventDetails

instance Core.NFData DescribeEventDetails

instance Core.ToHeaders DescribeEventDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEventDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventDetails where
  toJSON DescribeEventDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ ("locale" Core..=) Core.<$> locale,
            Core.Just ("eventArns" Core..= eventArns)
          ]
      )

instance Core.ToPath DescribeEventDetails where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventDetails where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventDetailsResponse' smart constructor.
data DescribeEventDetailsResponse = DescribeEventDetailsResponse'
  { -- | Information about the events that could be retrieved.
    successfulSet :: Core.Maybe [EventDetails],
    -- | Error messages for any events that could not be retrieved.
    failedSet :: Core.Maybe [EventDetailsErrorItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEventDetailsResponse
newDescribeEventDetailsResponse pHttpStatus_ =
  DescribeEventDetailsResponse'
    { successfulSet =
        Core.Nothing,
      failedSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the events that could be retrieved.
describeEventDetailsResponse_successfulSet :: Lens.Lens' DescribeEventDetailsResponse (Core.Maybe [EventDetails])
describeEventDetailsResponse_successfulSet = Lens.lens (\DescribeEventDetailsResponse' {successfulSet} -> successfulSet) (\s@DescribeEventDetailsResponse' {} a -> s {successfulSet = a} :: DescribeEventDetailsResponse) Core.. Lens.mapping Lens._Coerce

-- | Error messages for any events that could not be retrieved.
describeEventDetailsResponse_failedSet :: Lens.Lens' DescribeEventDetailsResponse (Core.Maybe [EventDetailsErrorItem])
describeEventDetailsResponse_failedSet = Lens.lens (\DescribeEventDetailsResponse' {failedSet} -> failedSet) (\s@DescribeEventDetailsResponse' {} a -> s {failedSet = a} :: DescribeEventDetailsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventDetailsResponse_httpStatus :: Lens.Lens' DescribeEventDetailsResponse Core.Int
describeEventDetailsResponse_httpStatus = Lens.lens (\DescribeEventDetailsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventDetailsResponse' {} a -> s {httpStatus = a} :: DescribeEventDetailsResponse)

instance Core.NFData DescribeEventDetailsResponse
