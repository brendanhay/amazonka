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
-- Module      : Amazonka.IoT.DescribeScheduledAudit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a scheduled audit.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeScheduledAudit>
-- action.
module Amazonka.IoT.DescribeScheduledAudit
  ( -- * Creating a Request
    DescribeScheduledAudit (..),
    newDescribeScheduledAudit,

    -- * Request Lenses
    describeScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    DescribeScheduledAuditResponse (..),
    newDescribeScheduledAuditResponse,

    -- * Response Lenses
    describeScheduledAuditResponse_dayOfMonth,
    describeScheduledAuditResponse_dayOfWeek,
    describeScheduledAuditResponse_frequency,
    describeScheduledAuditResponse_scheduledAuditArn,
    describeScheduledAuditResponse_scheduledAuditName,
    describeScheduledAuditResponse_targetCheckNames,
    describeScheduledAuditResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeScheduledAudit' smart constructor.
data DescribeScheduledAudit = DescribeScheduledAudit'
  { -- | The name of the scheduled audit whose information you want to get.
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeScheduledAudit
newDescribeScheduledAudit pScheduledAuditName_ =
  DescribeScheduledAudit'
    { scheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit whose information you want to get.
describeScheduledAudit_scheduledAuditName :: Lens.Lens' DescribeScheduledAudit Prelude.Text
describeScheduledAudit_scheduledAuditName = Lens.lens (\DescribeScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@DescribeScheduledAudit' {} a -> s {scheduledAuditName = a} :: DescribeScheduledAudit)

instance Core.AWSRequest DescribeScheduledAudit where
  type
    AWSResponse DescribeScheduledAudit =
      DescribeScheduledAuditResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduledAuditResponse'
            Prelude.<$> (x Data..?> "dayOfMonth")
            Prelude.<*> (x Data..?> "dayOfWeek")
            Prelude.<*> (x Data..?> "frequency")
            Prelude.<*> (x Data..?> "scheduledAuditArn")
            Prelude.<*> (x Data..?> "scheduledAuditName")
            Prelude.<*> ( x Data..?> "targetCheckNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScheduledAudit where
  hashWithSalt _salt DescribeScheduledAudit' {..} =
    _salt `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData DescribeScheduledAudit where
  rnf DescribeScheduledAudit' {..} =
    Prelude.rnf scheduledAuditName

instance Data.ToHeaders DescribeScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeScheduledAudit where
  toPath DescribeScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Data.toBS scheduledAuditName
      ]

instance Data.ToQuery DescribeScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScheduledAuditResponse' smart constructor.
data DescribeScheduledAuditResponse = DescribeScheduledAuditResponse'
  { -- | The day of the month on which the scheduled audit takes place. This is
    -- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
    -- the month does not have that many days, the audit takes place on the
    -- @LAST@ day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | The day of the week on which the scheduled audit takes place, either one
    -- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | How often the scheduled audit takes place, either one of @DAILY@,
    -- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
    -- determined by the system.
    frequency :: Prelude.Maybe AuditFrequency,
    -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled audit.
    scheduledAuditName :: Prelude.Maybe Prelude.Text,
    -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledAuditResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'describeScheduledAuditResponse_dayOfMonth' - The day of the month on which the scheduled audit takes place. This is
-- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
-- the month does not have that many days, the audit takes place on the
-- @LAST@ day of the month.
--
-- 'dayOfWeek', 'describeScheduledAuditResponse_dayOfWeek' - The day of the week on which the scheduled audit takes place, either one
-- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
--
-- 'frequency', 'describeScheduledAuditResponse_frequency' - How often the scheduled audit takes place, either one of @DAILY@,
-- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
-- determined by the system.
--
-- 'scheduledAuditArn', 'describeScheduledAuditResponse_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'scheduledAuditName', 'describeScheduledAuditResponse_scheduledAuditName' - The name of the scheduled audit.
--
-- 'targetCheckNames', 'describeScheduledAuditResponse_targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- 'httpStatus', 'describeScheduledAuditResponse_httpStatus' - The response's http status code.
newDescribeScheduledAuditResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScheduledAuditResponse
newDescribeScheduledAuditResponse pHttpStatus_ =
  DescribeScheduledAuditResponse'
    { dayOfMonth =
        Prelude.Nothing,
      dayOfWeek = Prelude.Nothing,
      frequency = Prelude.Nothing,
      scheduledAuditArn = Prelude.Nothing,
      scheduledAuditName = Prelude.Nothing,
      targetCheckNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The day of the month on which the scheduled audit takes place. This is
-- will be @1@ through @31@ or @LAST@. If days @29@-@31@ are specified, and
-- the month does not have that many days, the audit takes place on the
-- @LAST@ day of the month.
describeScheduledAuditResponse_dayOfMonth :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe Prelude.Text)
describeScheduledAuditResponse_dayOfMonth = Lens.lens (\DescribeScheduledAuditResponse' {dayOfMonth} -> dayOfMonth) (\s@DescribeScheduledAuditResponse' {} a -> s {dayOfMonth = a} :: DescribeScheduledAuditResponse)

-- | The day of the week on which the scheduled audit takes place, either one
-- of @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@.
describeScheduledAuditResponse_dayOfWeek :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe DayOfWeek)
describeScheduledAuditResponse_dayOfWeek = Lens.lens (\DescribeScheduledAuditResponse' {dayOfWeek} -> dayOfWeek) (\s@DescribeScheduledAuditResponse' {} a -> s {dayOfWeek = a} :: DescribeScheduledAuditResponse)

-- | How often the scheduled audit takes place, either one of @DAILY@,
-- @WEEKLY@, @BIWEEKLY@, or @MONTHLY@. The start time of each audit is
-- determined by the system.
describeScheduledAuditResponse_frequency :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe AuditFrequency)
describeScheduledAuditResponse_frequency = Lens.lens (\DescribeScheduledAuditResponse' {frequency} -> frequency) (\s@DescribeScheduledAuditResponse' {} a -> s {frequency = a} :: DescribeScheduledAuditResponse)

-- | The ARN of the scheduled audit.
describeScheduledAuditResponse_scheduledAuditArn :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe Prelude.Text)
describeScheduledAuditResponse_scheduledAuditArn = Lens.lens (\DescribeScheduledAuditResponse' {scheduledAuditArn} -> scheduledAuditArn) (\s@DescribeScheduledAuditResponse' {} a -> s {scheduledAuditArn = a} :: DescribeScheduledAuditResponse)

-- | The name of the scheduled audit.
describeScheduledAuditResponse_scheduledAuditName :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe Prelude.Text)
describeScheduledAuditResponse_scheduledAuditName = Lens.lens (\DescribeScheduledAuditResponse' {scheduledAuditName} -> scheduledAuditName) (\s@DescribeScheduledAuditResponse' {} a -> s {scheduledAuditName = a} :: DescribeScheduledAuditResponse)

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
describeScheduledAuditResponse_targetCheckNames :: Lens.Lens' DescribeScheduledAuditResponse (Prelude.Maybe [Prelude.Text])
describeScheduledAuditResponse_targetCheckNames = Lens.lens (\DescribeScheduledAuditResponse' {targetCheckNames} -> targetCheckNames) (\s@DescribeScheduledAuditResponse' {} a -> s {targetCheckNames = a} :: DescribeScheduledAuditResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScheduledAuditResponse_httpStatus :: Lens.Lens' DescribeScheduledAuditResponse Prelude.Int
describeScheduledAuditResponse_httpStatus = Lens.lens (\DescribeScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledAuditResponse' {} a -> s {httpStatus = a} :: DescribeScheduledAuditResponse)

instance
  Prelude.NFData
    DescribeScheduledAuditResponse
  where
  rnf DescribeScheduledAuditResponse' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf scheduledAuditArn
      `Prelude.seq` Prelude.rnf scheduledAuditName
      `Prelude.seq` Prelude.rnf targetCheckNames
      `Prelude.seq` Prelude.rnf httpStatus
