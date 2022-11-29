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
-- Module      : Amazonka.IoT.UpdateScheduledAudit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduled audit, including which checks are performed and how
-- often the audit takes place.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateScheduledAudit>
-- action.
module Amazonka.IoT.UpdateScheduledAudit
  ( -- * Creating a Request
    UpdateScheduledAudit (..),
    newUpdateScheduledAudit,

    -- * Request Lenses
    updateScheduledAudit_targetCheckNames,
    updateScheduledAudit_frequency,
    updateScheduledAudit_dayOfWeek,
    updateScheduledAudit_dayOfMonth,
    updateScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    UpdateScheduledAuditResponse (..),
    newUpdateScheduledAuditResponse,

    -- * Response Lenses
    updateScheduledAuditResponse_scheduledAuditArn,
    updateScheduledAuditResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Prelude.Maybe [Prelude.Text],
    -- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
    -- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
    -- the system.
    frequency :: Prelude.Maybe AuditFrequency,
    -- | The day of the week on which the scheduled audit takes place. This can
    -- be one of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field
    -- is required if the \"frequency\" parameter is set to @WEEKLY@ or
    -- @BIWEEKLY@.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The day of the month on which the scheduled audit takes place. This can
    -- be @1@ through @31@ or @LAST@. This field is required if the @frequency@
    -- parameter is set to @MONTHLY@. If days 29-31 are specified, and the
    -- month does not have that many days, the audit takes place on the
    -- \"LAST\" day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetCheckNames', 'updateScheduledAudit_targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- 'frequency', 'updateScheduledAudit_frequency' - How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
-- the system.
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
-- 'scheduledAuditName', 'updateScheduledAudit_scheduledAuditName' - The name of the scheduled audit. (Max. 128 chars)
newUpdateScheduledAudit ::
  -- | 'scheduledAuditName'
  Prelude.Text ->
  UpdateScheduledAudit
newUpdateScheduledAudit pScheduledAuditName_ =
  UpdateScheduledAudit'
    { targetCheckNames =
        Prelude.Nothing,
      frequency = Prelude.Nothing,
      dayOfWeek = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing,
      scheduledAuditName = pScheduledAuditName_
    }

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
updateScheduledAudit_targetCheckNames :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe [Prelude.Text])
updateScheduledAudit_targetCheckNames = Lens.lens (\UpdateScheduledAudit' {targetCheckNames} -> targetCheckNames) (\s@UpdateScheduledAudit' {} a -> s {targetCheckNames = a} :: UpdateScheduledAudit) Prelude.. Lens.mapping Lens.coerced

-- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
-- the system.
updateScheduledAudit_frequency :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe AuditFrequency)
updateScheduledAudit_frequency = Lens.lens (\UpdateScheduledAudit' {frequency} -> frequency) (\s@UpdateScheduledAudit' {} a -> s {frequency = a} :: UpdateScheduledAudit)

-- | The day of the week on which the scheduled audit takes place. This can
-- be one of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field
-- is required if the \"frequency\" parameter is set to @WEEKLY@ or
-- @BIWEEKLY@.
updateScheduledAudit_dayOfWeek :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe DayOfWeek)
updateScheduledAudit_dayOfWeek = Lens.lens (\UpdateScheduledAudit' {dayOfWeek} -> dayOfWeek) (\s@UpdateScheduledAudit' {} a -> s {dayOfWeek = a} :: UpdateScheduledAudit)

-- | The day of the month on which the scheduled audit takes place. This can
-- be @1@ through @31@ or @LAST@. This field is required if the @frequency@
-- parameter is set to @MONTHLY@. If days 29-31 are specified, and the
-- month does not have that many days, the audit takes place on the
-- \"LAST\" day of the month.
updateScheduledAudit_dayOfMonth :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe Prelude.Text)
updateScheduledAudit_dayOfMonth = Lens.lens (\UpdateScheduledAudit' {dayOfMonth} -> dayOfMonth) (\s@UpdateScheduledAudit' {} a -> s {dayOfMonth = a} :: UpdateScheduledAudit)

-- | The name of the scheduled audit. (Max. 128 chars)
updateScheduledAudit_scheduledAuditName :: Lens.Lens' UpdateScheduledAudit Prelude.Text
updateScheduledAudit_scheduledAuditName = Lens.lens (\UpdateScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@UpdateScheduledAudit' {} a -> s {scheduledAuditName = a} :: UpdateScheduledAudit)

instance Core.AWSRequest UpdateScheduledAudit where
  type
    AWSResponse UpdateScheduledAudit =
      UpdateScheduledAuditResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            Prelude.<$> (x Core..?> "scheduledAuditArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateScheduledAudit where
  hashWithSalt _salt UpdateScheduledAudit' {..} =
    _salt `Prelude.hashWithSalt` targetCheckNames
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData UpdateScheduledAudit where
  rnf UpdateScheduledAudit' {..} =
    Prelude.rnf targetCheckNames
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf scheduledAuditName

instance Core.ToHeaders UpdateScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetCheckNames" Core..=)
              Prelude.<$> targetCheckNames,
            ("frequency" Core..=) Prelude.<$> frequency,
            ("dayOfWeek" Core..=) Prelude.<$> dayOfWeek,
            ("dayOfMonth" Core..=) Prelude.<$> dayOfMonth
          ]
      )

instance Core.ToPath UpdateScheduledAudit where
  toPath UpdateScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Core.toBS scheduledAuditName
      ]

instance Core.ToQuery UpdateScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateScheduledAuditResponse
newUpdateScheduledAuditResponse pHttpStatus_ =
  UpdateScheduledAuditResponse'
    { scheduledAuditArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the scheduled audit.
updateScheduledAuditResponse_scheduledAuditArn :: Lens.Lens' UpdateScheduledAuditResponse (Prelude.Maybe Prelude.Text)
updateScheduledAuditResponse_scheduledAuditArn = Lens.lens (\UpdateScheduledAuditResponse' {scheduledAuditArn} -> scheduledAuditArn) (\s@UpdateScheduledAuditResponse' {} a -> s {scheduledAuditArn = a} :: UpdateScheduledAuditResponse)

-- | The response's http status code.
updateScheduledAuditResponse_httpStatus :: Lens.Lens' UpdateScheduledAuditResponse Prelude.Int
updateScheduledAuditResponse_httpStatus = Lens.lens (\UpdateScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@UpdateScheduledAuditResponse' {} a -> s {httpStatus = a} :: UpdateScheduledAuditResponse)

instance Prelude.NFData UpdateScheduledAuditResponse where
  rnf UpdateScheduledAuditResponse' {..} =
    Prelude.rnf scheduledAuditArn
      `Prelude.seq` Prelude.rnf httpStatus
