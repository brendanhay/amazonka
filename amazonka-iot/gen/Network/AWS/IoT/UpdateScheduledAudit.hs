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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { -- | The day of the week on which the scheduled audit takes place. This can
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
    -- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
    -- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
    -- the system.
    frequency :: Prelude.Maybe AuditFrequency,
    -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateScheduledAudit
newUpdateScheduledAudit pScheduledAuditName_ =
  UpdateScheduledAudit'
    { dayOfWeek = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing,
      frequency = Prelude.Nothing,
      targetCheckNames = Prelude.Nothing,
      scheduledAuditName = pScheduledAuditName_
    }

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

-- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@, or @MONTHLY@. The start time of each audit is determined by
-- the system.
updateScheduledAudit_frequency :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe AuditFrequency)
updateScheduledAudit_frequency = Lens.lens (\UpdateScheduledAudit' {frequency} -> frequency) (\s@UpdateScheduledAudit' {} a -> s {frequency = a} :: UpdateScheduledAudit)

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
updateScheduledAudit_targetCheckNames :: Lens.Lens' UpdateScheduledAudit (Prelude.Maybe [Prelude.Text])
updateScheduledAudit_targetCheckNames = Lens.lens (\UpdateScheduledAudit' {targetCheckNames} -> targetCheckNames) (\s@UpdateScheduledAudit' {} a -> s {targetCheckNames = a} :: UpdateScheduledAudit) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the scheduled audit. (Max. 128 chars)
updateScheduledAudit_scheduledAuditName :: Lens.Lens' UpdateScheduledAudit Prelude.Text
updateScheduledAudit_scheduledAuditName = Lens.lens (\UpdateScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@UpdateScheduledAudit' {} a -> s {scheduledAuditName = a} :: UpdateScheduledAudit)

instance Prelude.AWSRequest UpdateScheduledAudit where
  type
    Rs UpdateScheduledAudit =
      UpdateScheduledAuditResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            Prelude.<$> (x Prelude..?> "scheduledAuditArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateScheduledAudit

instance Prelude.NFData UpdateScheduledAudit

instance Prelude.ToHeaders UpdateScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dayOfWeek" Prelude..=) Prelude.<$> dayOfWeek,
            ("dayOfMonth" Prelude..=) Prelude.<$> dayOfMonth,
            ("frequency" Prelude..=) Prelude.<$> frequency,
            ("targetCheckNames" Prelude..=)
              Prelude.<$> targetCheckNames
          ]
      )

instance Prelude.ToPath UpdateScheduledAudit where
  toPath UpdateScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Prelude.toBS scheduledAuditName
      ]

instance Prelude.ToQuery UpdateScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateScheduledAuditResponse
