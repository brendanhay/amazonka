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
-- Module      : Network.AWS.IoT.UpdateScheduledAudit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduled audit, including which checks are performed and how
-- often the audit takes place.
module Network.AWS.IoT.UpdateScheduledAudit
  ( -- * Creating a Request
    UpdateScheduledAudit (..),
    newUpdateScheduledAudit,

    -- * Request Lenses
    updateScheduledAudit_dayOfWeek,
    updateScheduledAudit_dayOfMonth,
    updateScheduledAudit_frequency,
    updateScheduledAudit_targetCheckNames,
    updateScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    UpdateScheduledAuditResponse (..),
    newUpdateScheduledAuditResponse,

    -- * Response Lenses
    updateScheduledAuditResponse_scheduledAuditArn,
    updateScheduledAuditResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { -- | The day of the week on which the scheduled audit takes place. This can
    -- be one of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field
    -- is required if the \"frequency\" parameter is set to @WEEKLY@ or
    -- @BIWEEKLY@.
    dayOfWeek :: Core.Maybe DayOfWeek,
    -- | The day of the month on which the scheduled audit takes place. This can
    -- be @1@ through @31@ or @LAST@. This field is required if the @frequency@
    -- parameter is set to @MONTHLY@. If days 29-31 are specified, and the
    -- month does not have that many days, the audit takes place on the
    -- \"LAST\" day of the month.
    dayOfMonth :: Core.Maybe Core.Text,
    -- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
    -- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
    -- the system.
    frequency :: Core.Maybe AuditFrequency,
    -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Core.Maybe [Core.Text],
    -- | The name of the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'updateScheduledAudit_dayOfWeek' - The day of the week on which the scheduled audit takes place. This can
-- be one of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field
-- is required if the \"frequency\" parameter is set to @WEEKLY@ or
-- @BIWEEKLY@.
--
-- 'dayOfMonth', 'updateScheduledAudit_dayOfMonth' - The day of the month on which the scheduled audit takes place. This can
-- be @1@ through @31@ or @LAST@. This field is required if the @frequency@
-- parameter is set to @MONTHLY@. If days 29-31 are specified, and the
-- month does not have that many days, the audit takes place on the
-- \"LAST\" day of the month.
--
-- 'frequency', 'updateScheduledAudit_frequency' - How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
-- the system.
--
-- 'targetCheckNames', 'updateScheduledAudit_targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- 'scheduledAuditName', 'updateScheduledAudit_scheduledAuditName' - The name of the scheduled audit. (Max. 128 chars)
newUpdateScheduledAudit ::
  -- | 'scheduledAuditName'
  Core.Text ->
  UpdateScheduledAudit
newUpdateScheduledAudit pScheduledAuditName_ =
  UpdateScheduledAudit'
    { dayOfWeek = Core.Nothing,
      dayOfMonth = Core.Nothing,
      frequency = Core.Nothing,
      targetCheckNames = Core.Nothing,
      scheduledAuditName = pScheduledAuditName_
    }

-- | The day of the week on which the scheduled audit takes place. This can
-- be one of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field
-- is required if the \"frequency\" parameter is set to @WEEKLY@ or
-- @BIWEEKLY@.
updateScheduledAudit_dayOfWeek :: Lens.Lens' UpdateScheduledAudit (Core.Maybe DayOfWeek)
updateScheduledAudit_dayOfWeek = Lens.lens (\UpdateScheduledAudit' {dayOfWeek} -> dayOfWeek) (\s@UpdateScheduledAudit' {} a -> s {dayOfWeek = a} :: UpdateScheduledAudit)

-- | The day of the month on which the scheduled audit takes place. This can
-- be @1@ through @31@ or @LAST@. This field is required if the @frequency@
-- parameter is set to @MONTHLY@. If days 29-31 are specified, and the
-- month does not have that many days, the audit takes place on the
-- \"LAST\" day of the month.
updateScheduledAudit_dayOfMonth :: Lens.Lens' UpdateScheduledAudit (Core.Maybe Core.Text)
updateScheduledAudit_dayOfMonth = Lens.lens (\UpdateScheduledAudit' {dayOfMonth} -> dayOfMonth) (\s@UpdateScheduledAudit' {} a -> s {dayOfMonth = a} :: UpdateScheduledAudit)

-- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
-- the system.
updateScheduledAudit_frequency :: Lens.Lens' UpdateScheduledAudit (Core.Maybe AuditFrequency)
updateScheduledAudit_frequency = Lens.lens (\UpdateScheduledAudit' {frequency} -> frequency) (\s@UpdateScheduledAudit' {} a -> s {frequency = a} :: UpdateScheduledAudit)

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
updateScheduledAudit_targetCheckNames :: Lens.Lens' UpdateScheduledAudit (Core.Maybe [Core.Text])
updateScheduledAudit_targetCheckNames = Lens.lens (\UpdateScheduledAudit' {targetCheckNames} -> targetCheckNames) (\s@UpdateScheduledAudit' {} a -> s {targetCheckNames = a} :: UpdateScheduledAudit) Core.. Lens.mapping Lens._Coerce

-- | The name of the scheduled audit. (Max. 128 chars)
updateScheduledAudit_scheduledAuditName :: Lens.Lens' UpdateScheduledAudit Core.Text
updateScheduledAudit_scheduledAuditName = Lens.lens (\UpdateScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@UpdateScheduledAudit' {} a -> s {scheduledAuditName = a} :: UpdateScheduledAudit)

instance Core.AWSRequest UpdateScheduledAudit where
  type
    AWSResponse UpdateScheduledAudit =
      UpdateScheduledAuditResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            Core.<$> (x Core..?> "scheduledAuditArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateScheduledAudit

instance Core.NFData UpdateScheduledAudit

instance Core.ToHeaders UpdateScheduledAudit where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("dayOfWeek" Core..=) Core.<$> dayOfWeek,
            ("dayOfMonth" Core..=) Core.<$> dayOfMonth,
            ("frequency" Core..=) Core.<$> frequency,
            ("targetCheckNames" Core..=)
              Core.<$> targetCheckNames
          ]
      )

instance Core.ToPath UpdateScheduledAudit where
  toPath UpdateScheduledAudit' {..} =
    Core.mconcat
      [ "/audit/scheduledaudits/",
        Core.toBS scheduledAuditName
      ]

instance Core.ToQuery UpdateScheduledAudit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateScheduledAuditResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAuditArn', 'updateScheduledAuditResponse_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'httpStatus', 'updateScheduledAuditResponse_httpStatus' - The response's http status code.
newUpdateScheduledAuditResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateScheduledAuditResponse
newUpdateScheduledAuditResponse pHttpStatus_ =
  UpdateScheduledAuditResponse'
    { scheduledAuditArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the scheduled audit.
updateScheduledAuditResponse_scheduledAuditArn :: Lens.Lens' UpdateScheduledAuditResponse (Core.Maybe Core.Text)
updateScheduledAuditResponse_scheduledAuditArn = Lens.lens (\UpdateScheduledAuditResponse' {scheduledAuditArn} -> scheduledAuditArn) (\s@UpdateScheduledAuditResponse' {} a -> s {scheduledAuditArn = a} :: UpdateScheduledAuditResponse)

-- | The response's http status code.
updateScheduledAuditResponse_httpStatus :: Lens.Lens' UpdateScheduledAuditResponse Core.Int
updateScheduledAuditResponse_httpStatus = Lens.lens (\UpdateScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@UpdateScheduledAuditResponse' {} a -> s {httpStatus = a} :: UpdateScheduledAuditResponse)

instance Core.NFData UpdateScheduledAuditResponse
