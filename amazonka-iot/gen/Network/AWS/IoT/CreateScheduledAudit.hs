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
-- Module      : Network.AWS.IoT.CreateScheduledAudit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled audit that is run at a specified time interval.
module Network.AWS.IoT.CreateScheduledAudit
  ( -- * Creating a Request
    CreateScheduledAudit (..),
    newCreateScheduledAudit,

    -- * Request Lenses
    createScheduledAudit_dayOfWeek,
    createScheduledAudit_dayOfMonth,
    createScheduledAudit_tags,
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateScheduledAudit' smart constructor.
data CreateScheduledAudit = CreateScheduledAudit'
  { -- | The day of the week on which the scheduled audit takes place, either
    -- @SUN@, @MON@, @TUE@, @WED@, @THU@, @FRI@, or @SAT@. This field is
    -- required if the @frequency@ parameter is set to @WEEKLY@ or @BIWEEKLY@.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | The day of the month on which the scheduled audit takes place. This can
    -- be \"1\" through \"31\" or \"LAST\". This field is required if the
    -- \"frequency\" parameter is set to @MONTHLY@. If days 29 to 31 are
    -- specified, and the month doesn\'t have that many days, the audit takes
    -- place on the @LAST@ day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | Metadata that can be used to manage the scheduled audit.
    tags :: Prelude.Maybe [Tag],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledAudit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'tags', 'createScheduledAudit_tags' - Metadata that can be used to manage the scheduled audit.
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
      { dayOfWeek = Prelude.Nothing,
        dayOfMonth = Prelude.Nothing,
        tags = Prelude.Nothing,
        frequency = pFrequency_,
        targetCheckNames = Prelude.mempty,
        scheduledAuditName = pScheduledAuditName_
      }

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

-- | Metadata that can be used to manage the scheduled audit.
createScheduledAudit_tags :: Lens.Lens' CreateScheduledAudit (Prelude.Maybe [Tag])
createScheduledAudit_tags = Lens.lens (\CreateScheduledAudit' {tags} -> tags) (\s@CreateScheduledAudit' {} a -> s {tags = a} :: CreateScheduledAudit) Prelude.. Lens.mapping Prelude._Coerce

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
createScheduledAudit_targetCheckNames = Lens.lens (\CreateScheduledAudit' {targetCheckNames} -> targetCheckNames) (\s@CreateScheduledAudit' {} a -> s {targetCheckNames = a} :: CreateScheduledAudit) Prelude.. Prelude._Coerce

-- | The name you want to give to the scheduled audit. (Max. 128 chars)
createScheduledAudit_scheduledAuditName :: Lens.Lens' CreateScheduledAudit Prelude.Text
createScheduledAudit_scheduledAuditName = Lens.lens (\CreateScheduledAudit' {scheduledAuditName} -> scheduledAuditName) (\s@CreateScheduledAudit' {} a -> s {scheduledAuditName = a} :: CreateScheduledAudit)

instance Prelude.AWSRequest CreateScheduledAudit where
  type
    Rs CreateScheduledAudit =
      CreateScheduledAuditResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduledAuditResponse'
            Prelude.<$> (x Prelude..?> "scheduledAuditArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScheduledAudit

instance Prelude.NFData CreateScheduledAudit

instance Prelude.ToHeaders CreateScheduledAudit where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateScheduledAudit where
  toJSON CreateScheduledAudit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dayOfWeek" Prelude..=) Prelude.<$> dayOfWeek,
            ("dayOfMonth" Prelude..=) Prelude.<$> dayOfMonth,
            ("tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("frequency" Prelude..= frequency),
            Prelude.Just
              ("targetCheckNames" Prelude..= targetCheckNames)
          ]
      )

instance Prelude.ToPath CreateScheduledAudit where
  toPath CreateScheduledAudit' {..} =
    Prelude.mconcat
      [ "/audit/scheduledaudits/",
        Prelude.toBS scheduledAuditName
      ]

instance Prelude.ToQuery CreateScheduledAudit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduledAuditResponse' smart constructor.
data CreateScheduledAuditResponse = CreateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateScheduledAuditResponse
