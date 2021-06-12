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
-- Module      : Network.AWS.IoT.DescribeScheduledAudit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a scheduled audit.
module Network.AWS.IoT.DescribeScheduledAudit
  ( -- * Creating a Request
    DescribeScheduledAudit (..),
    newDescribeScheduledAudit,

    -- * Request Lenses
    describeScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    DescribeScheduledAuditResponse (..),
    newDescribeScheduledAuditResponse,

    -- * Response Lenses
    describeScheduledAuditResponse_dayOfWeek,
    describeScheduledAuditResponse_scheduledAuditArn,
    describeScheduledAuditResponse_scheduledAuditName,
    describeScheduledAuditResponse_dayOfMonth,
    describeScheduledAuditResponse_frequency,
    describeScheduledAuditResponse_targetCheckNames,
    describeScheduledAuditResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScheduledAudit' smart constructor.
data DescribeScheduledAudit = DescribeScheduledAudit'
  { -- | The name of the scheduled audit whose information you want to get.
    scheduledAuditName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAuditName', 'describeScheduledAudit_scheduledAuditName' - The name of the scheduled audit whose information you want to get.
newDescribeScheduledAudit ::
  -- | 'scheduledAuditName'
  Core.Text ->
  DescribeScheduledAudit
newDescribeScheduledAudit pScheduledAuditName_ =
  DescribeScheduledAudit'
    { scheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit whose information you want to get.
describeScheduledAudit_scheduledAuditName :: Lens.Lens' DescribeScheduledAudit Core.Text
describeScheduledAudit_scheduledAuditName = Lens.lens (\DescribeScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@DescribeScheduledAudit' {} a -> s {scheduledAuditName = a} :: DescribeScheduledAudit)

instance Core.AWSRequest DescribeScheduledAudit where
  type
    AWSResponse DescribeScheduledAudit =
      DescribeScheduledAuditResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduledAuditResponse'
            Core.<$> (x Core..?> "dayOfWeek")
            Core.<*> (x Core..?> "scheduledAuditArn")
            Core.<*> (x Core..?> "scheduledAuditName")
            Core.<*> (x Core..?> "dayOfMonth")
            Core.<*> (x Core..?> "frequency")
            Core.<*> (x Core..?> "targetCheckNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeScheduledAudit

instance Core.NFData DescribeScheduledAudit

instance Core.ToHeaders DescribeScheduledAudit where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeScheduledAudit where
  toPath DescribeScheduledAudit' {..} =
    Core.mconcat
      [ "/audit/scheduledaudits/",
        Core.toBS scheduledAuditName
      ]

instance Core.ToQuery DescribeScheduledAudit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeScheduledAuditResponse' smart constructor.
data DescribeScheduledAuditResponse = DescribeScheduledAuditResponse'
  { -- | The day of the week on which the scheduled audit takes place, either one
    -- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
    dayOfWeek :: Core.Maybe DayOfWeek,
    -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Core.Maybe Core.Text,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Core.Maybe Core.Text,
    -- | The day of the month on which the scheduled audit takes place. This is
    -- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
    -- the month does not have that many days, the audit takes place on the
    -- @LAST@ day of the month.
    dayOfMonth :: Core.Maybe Core.Text,
    -- | How often the scheduled audit takes place, either one of @DAILY@,
    -- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
    -- determined by the system.
    frequency :: Core.Maybe AuditFrequency,
    -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScheduledAuditResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'describeScheduledAuditResponse_dayOfWeek' - The day of the week on which the scheduled audit takes place, either one
-- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
--
-- 'scheduledAuditArn', 'describeScheduledAuditResponse_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'scheduledAuditName', 'describeScheduledAuditResponse_scheduledAuditName' - The name of the scheduled audit.
--
-- 'dayOfMonth', 'describeScheduledAuditResponse_dayOfMonth' - The day of the month on which the scheduled audit takes place. This is
-- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
-- the month does not have that many days, the audit takes place on the
-- @LAST@ day of the month.
--
-- 'frequency', 'describeScheduledAuditResponse_frequency' - How often the scheduled audit takes place, either one of @DAILY@,
-- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
-- determined by the system.
--
-- 'targetCheckNames', 'describeScheduledAuditResponse_targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- 'httpStatus', 'describeScheduledAuditResponse_httpStatus' - The response's http status code.
newDescribeScheduledAuditResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeScheduledAuditResponse
newDescribeScheduledAuditResponse pHttpStatus_ =
  DescribeScheduledAuditResponse'
    { dayOfWeek =
        Core.Nothing,
      scheduledAuditArn = Core.Nothing,
      scheduledAuditName = Core.Nothing,
      dayOfMonth = Core.Nothing,
      frequency = Core.Nothing,
      targetCheckNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The day of the week on which the scheduled audit takes place, either one
-- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
describeScheduledAuditResponse_dayOfWeek :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe DayOfWeek)
describeScheduledAuditResponse_dayOfWeek = Lens.lens (\DescribeScheduledAuditResponse' {dayOfWeek} -> dayOfWeek) (\s@DescribeScheduledAuditResponse' {} a -> s {dayOfWeek = a} :: DescribeScheduledAuditResponse)

-- | The ARN of the scheduled audit.
describeScheduledAuditResponse_scheduledAuditArn :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe Core.Text)
describeScheduledAuditResponse_scheduledAuditArn = Lens.lens (\DescribeScheduledAuditResponse' {scheduledAuditArn} -> scheduledAuditArn) (\s@DescribeScheduledAuditResponse' {} a -> s {scheduledAuditArn = a} :: DescribeScheduledAuditResponse)

-- | The name of the scheduled audit.
describeScheduledAuditResponse_scheduledAuditName :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe Core.Text)
describeScheduledAuditResponse_scheduledAuditName = Lens.lens (\DescribeScheduledAuditResponse' {scheduledAuditName} -> scheduledAuditName) (\s@DescribeScheduledAuditResponse' {} a -> s {scheduledAuditName = a} :: DescribeScheduledAuditResponse)

-- | The day of the month on which the scheduled audit takes place. This is
-- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
-- the month does not have that many days, the audit takes place on the
-- @LAST@ day of the month.
describeScheduledAuditResponse_dayOfMonth :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe Core.Text)
describeScheduledAuditResponse_dayOfMonth = Lens.lens (\DescribeScheduledAuditResponse' {dayOfMonth} -> dayOfMonth) (\s@DescribeScheduledAuditResponse' {} a -> s {dayOfMonth = a} :: DescribeScheduledAuditResponse)

-- | How often the scheduled audit takes place, either one of @DAILY@,
-- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
-- determined by the system.
describeScheduledAuditResponse_frequency :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe AuditFrequency)
describeScheduledAuditResponse_frequency = Lens.lens (\DescribeScheduledAuditResponse' {frequency} -> frequency) (\s@DescribeScheduledAuditResponse' {} a -> s {frequency = a} :: DescribeScheduledAuditResponse)

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
describeScheduledAuditResponse_targetCheckNames :: Lens.Lens' DescribeScheduledAuditResponse (Core.Maybe [Core.Text])
describeScheduledAuditResponse_targetCheckNames = Lens.lens (\DescribeScheduledAuditResponse' {targetCheckNames} -> targetCheckNames) (\s@DescribeScheduledAuditResponse' {} a -> s {targetCheckNames = a} :: DescribeScheduledAuditResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeScheduledAuditResponse_httpStatus :: Lens.Lens' DescribeScheduledAuditResponse Core.Int
describeScheduledAuditResponse_httpStatus = Lens.lens (\DescribeScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledAuditResponse' {} a -> s {httpStatus = a} :: DescribeScheduledAuditResponse)

instance Core.NFData DescribeScheduledAuditResponse
