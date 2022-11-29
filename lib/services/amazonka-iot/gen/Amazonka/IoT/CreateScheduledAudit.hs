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
-- Module      : Amazonka.IoT.CreateScheduledAudit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled audit that is run at a specified time interval.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateScheduledAudit>
-- action.
module Amazonka.IoT.CreateScheduledAudit
  ( -- * Creating a Request
    CreateScheduledAudit (..),
    newCreateScheduledAudit,

    -- * Request Lenses
    createScheduledAudit_tags,
    createScheduledAudit_dayOfWeek,
    createScheduledAudit_dayOfMonth,
    createScheduledAudit_frequency,
    createScheduledAudit_targetCheckNames,
    createScheduledAudit_scheduledAuditName,

    -- * Destructuring the Response
    CreateScheduledAuditResponse (..),
    newCreateScheduledAuditResponse,

    -- * Response Lenses
    createScheduledAuditResponse_scheduledAuditArn,
    createScheduledAuditResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateScheduledAudit' smart constructor.
data CreateScheduledAudit = CreateScheduledAudit'
  { -- | Metadata that can be used to manage the scheduled audit.
    tags :: Prelude.Maybe [Tag],
    -- | The day of the week on which the scheduled audit takes place, either
    -- @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field is
    -- required if the @frequency@ parameter is set to @WEEKLY@ or @BIWEEKLY@.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The day of the month on which the scheduled audit takes place. This can
    -- be \"1\" through \"31\" or \"LAST\". This field is required if the
    -- \"frequency\" parameter is set to @MONTHLY@. If days 29 to 31 are
    -- specified, and the month doesn\'t have that many days, the audit takes
    -- place on the @LAST@ day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
    -- @BIWEEKLY@ or @MONTHLY@. The start time of each audit is determined by
    -- the system.
    frequency :: AuditFrequency,
    -- | Which checks are performed during the scheduled audit. Checks must be
    -- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
    -- see the list of all checks, including those that are enabled or use
    -- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: [Prelude.Text],
    -- | The name you want to give to the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createScheduledAudit_tags' - Metadata that can be used to manage the scheduled audit.
--
-- 'dayOfWeek', 'createScheduledAudit_dayOfWeek' - The day of the week on which the scheduled audit takes place, either
-- @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field is
-- required if the @frequency@ parameter is set to @WEEKLY@ or @BIWEEKLY@.
--
-- 'dayOfMonth', 'createScheduledAudit_dayOfMonth' - The day of the month on which the scheduled audit takes place. This can
-- be \"1\" through \"31\" or \"LAST\". This field is required if the
-- \"frequency\" parameter is set to @MONTHLY@. If days 29 to 31 are
-- specified, and the month doesn\'t have that many days, the audit takes
-- place on the @LAST@ day of the month.
--
-- 'frequency', 'createScheduledAudit_frequency' - How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@ or @MONTHLY@. The start time of each audit is determined by
-- the system.
--
-- 'targetCheckNames', 'createScheduledAudit_targetCheckNames' - Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- 'scheduledAuditName', 'createScheduledAudit_scheduledAuditName' - The name you want to give to the scheduled audit. (Max. 128 chars)
newCreateScheduledAudit ::
  -- | 'frequency'
  AuditFrequency ->
  -- | 'scheduledAuditName'
  Prelude.Text ->
  CreateScheduledAudit
newCreateScheduledAudit
  pFrequency_
  pScheduledAuditName_ =
    CreateScheduledAudit'
      { tags = Prelude.Nothing,
        dayOfWeek = Prelude.Nothing,
        dayOfMonth = Prelude.Nothing,
        frequency = pFrequency_,
        targetCheckNames = Prelude.mempty,
        scheduledAuditName = pScheduledAuditName_
      }

-- | Metadata that can be used to manage the scheduled audit.
createScheduledAudit_tags :: Lens.Lens' CreateScheduledAudit (Prelude.Maybe [Tag])
createScheduledAudit_tags = Lens.lens (\CreateScheduledAudit' {tags} -> tags) (\s@CreateScheduledAudit' {} a -> s {tags = a} :: CreateScheduledAudit) Prelude.. Lens.mapping Lens.coerced

-- | The day of the week on which the scheduled audit takes place, either
-- @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field is
-- required if the @frequency@ parameter is set to @WEEKLY@ or @BIWEEKLY@.
createScheduledAudit_dayOfWeek :: Lens.Lens' CreateScheduledAudit (Prelude.Maybe DayOfWeek)
createScheduledAudit_dayOfWeek = Lens.lens (\CreateScheduledAudit' {dayOfWeek} -> dayOfWeek) (\s@CreateScheduledAudit' {} a -> s {dayOfWeek = a} :: CreateScheduledAudit)

-- | The day of the month on which the scheduled audit takes place. This can
-- be \"1\" through \"31\" or \"LAST\". This field is required if the
-- \"frequency\" parameter is set to @MONTHLY@. If days 29 to 31 are
-- specified, and the month doesn\'t have that many days, the audit takes
-- place on the @LAST@ day of the month.
createScheduledAudit_dayOfMonth :: Lens.Lens' CreateScheduledAudit (Prelude.Maybe Prelude.Text)
createScheduledAudit_dayOfMonth = Lens.lens (\CreateScheduledAudit' {dayOfMonth} -> dayOfMonth) (\s@CreateScheduledAudit' {} a -> s {dayOfMonth = a} :: CreateScheduledAudit)

-- | How often the scheduled audit takes place, either @DAILY@, @WEEKLY@,
-- @BIWEEKLY@ or @MONTHLY@. The start time of each audit is determined by
-- the system.
createScheduledAudit_frequency :: Lens.Lens' CreateScheduledAudit AuditFrequency
createScheduledAudit_frequency = Lens.lens (\CreateScheduledAudit' {frequency} -> frequency) (\s@CreateScheduledAudit' {} a -> s {frequency = a} :: CreateScheduledAudit)

-- | Which checks are performed during the scheduled audit. Checks must be
-- enabled for your account. (Use @DescribeAccountAuditConfiguration@ to
-- see the list of all checks, including those that are enabled or use
-- @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
createScheduledAudit_targetCheckNames :: Lens.Lens' CreateScheduledAudit [Prelude.Text]
createScheduledAudit_targetCheckNames = Lens.lens (\CreateScheduledAudit' {targetCheckNames} -> targetCheckNames) (\s@CreateScheduledAudit' {} a -> s {targetCheckNames = a} :: CreateScheduledAudit) Prelude.. Lens.coerced

-- | The name you want to give to the scheduled audit. (Max. 128 chars)
createScheduledAudit_scheduledAuditName :: Lens.Lens' CreateScheduledAudit Prelude.Text
createScheduledAudit_scheduledAuditName = Lens.lens (\CreateScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@CreateScheduledAudit' {} a -> s {scheduledAuditName = a} :: CreateScheduledAudit)

instance Core.AWSRequest CreateScheduledAudit where
  type
    AWSResponse CreateScheduledAudit =
      CreateScheduledAuditResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduledAuditResponse'
            Prelude.<$> (x Core..?> "scheduledAuditArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScheduledAudit where
  hashWithSalt _salt CreateScheduledAudit' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` targetCheckNames
      `Prelude.hashWithSalt` scheduledAuditName

instance Prelude.NFData CreateScheduledAudit where
  rnf CreateScheduledAudit' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf targetCheckNames
      `Prelude.seq` Prelude.rnf scheduledAuditName

instance Core.ToHeaders CreateScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateScheduledAudit where
  toJSON CreateScheduledAudit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("dayOfWeek" Core..=) Prelude.<$> dayOfWeek,
            ("dayOfMonth" Core..=) Prelude.<$> dayOfMonth,
            Prelude.Just ("frequency" Core..= frequency),
            Prelude.Just
              ("targetCheckNames" Core..= targetCheckNames)
          ]
      )

instance Core.ToPath CreateScheduledAudit where
  toPath CreateScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Core.toBS scheduledAuditName
      ]

instance Core.ToQuery CreateScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduledAuditResponse' smart constructor.
data CreateScheduledAuditResponse = CreateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledAuditResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAuditArn', 'createScheduledAuditResponse_scheduledAuditArn' - The ARN of the scheduled audit.
--
-- 'httpStatus', 'createScheduledAuditResponse_httpStatus' - The response's http status code.
newCreateScheduledAuditResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateScheduledAuditResponse
newCreateScheduledAuditResponse pHttpStatus_ =
  CreateScheduledAuditResponse'
    { scheduledAuditArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the scheduled audit.
createScheduledAuditResponse_scheduledAuditArn :: Lens.Lens' CreateScheduledAuditResponse (Prelude.Maybe Prelude.Text)
createScheduledAuditResponse_scheduledAuditArn = Lens.lens (\CreateScheduledAuditResponse' {scheduledAuditArn} -> scheduledAuditArn) (\s@CreateScheduledAuditResponse' {} a -> s {scheduledAuditArn = a} :: CreateScheduledAuditResponse)

-- | The response's http status code.
createScheduledAuditResponse_httpStatus :: Lens.Lens' CreateScheduledAuditResponse Prelude.Int
createScheduledAuditResponse_httpStatus = Lens.lens (\CreateScheduledAuditResponse' {httpStatus} -> httpStatus) (\s@CreateScheduledAuditResponse' {} a -> s {httpStatus = a} :: CreateScheduledAuditResponse)

instance Prelude.NFData CreateScheduledAuditResponse where
  rnf CreateScheduledAuditResponse' {..} =
    Prelude.rnf scheduledAuditArn
      `Prelude.seq` Prelude.rnf httpStatus
