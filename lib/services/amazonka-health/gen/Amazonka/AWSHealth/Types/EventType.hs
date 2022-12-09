{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AWSHealth.Types.EventType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventType where

import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the metadata about a type of event that is reported by Health.
-- The @EventType@ shows the category, service, and the event type code of
-- the event. For example, an @issue@ might be the category, @EC2@ the
-- service, and @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ the event type code.
--
-- You can use the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventTypes.html DescribeEventTypes>
-- API operation to return this information about an event.
--
-- You can also use the Amazon CloudWatch Events console to create a rule
-- so that you can get notified or take action when Health delivers a
-- specific event to your Amazon Web Services account. For more
-- information, see
-- <https://docs.aws.amazon.com/health/latest/ug/cloudwatch-events-health.html Monitor for Health events with Amazon CloudWatch Events>
-- in the /Health User Guide/.
--
-- /See:/ 'newEventType' smart constructor.
data EventType = EventType'
  { -- | A list of event type category codes. Possible values are @issue@,
    -- @accountNotification@, or @scheduledChange@. Currently, the
    -- @investigation@ value isn\'t supported at this time.
    category :: Prelude.Maybe EventTypeCategory,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION @; for example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service that is affected by the event. For
    -- example, @EC2@, @RDS@.
    service :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'eventType_category' - A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
--
-- 'code', 'eventType_code' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'service', 'eventType_service' - The Amazon Web Services service that is affected by the event. For
-- example, @EC2@, @RDS@.
newEventType ::
  EventType
newEventType =
  EventType'
    { category = Prelude.Nothing,
      code = Prelude.Nothing,
      service = Prelude.Nothing
    }

-- | A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
eventType_category :: Lens.Lens' EventType (Prelude.Maybe EventTypeCategory)
eventType_category = Lens.lens (\EventType' {category} -> category) (\s@EventType' {} a -> s {category = a} :: EventType)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
eventType_code :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_code = Lens.lens (\EventType' {code} -> code) (\s@EventType' {} a -> s {code = a} :: EventType)

-- | The Amazon Web Services service that is affected by the event. For
-- example, @EC2@, @RDS@.
eventType_service :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_service = Lens.lens (\EventType' {service} -> service) (\s@EventType' {} a -> s {service = a} :: EventType)

instance Data.FromJSON EventType where
  parseJSON =
    Data.withObject
      "EventType"
      ( \x ->
          EventType'
            Prelude.<$> (x Data..:? "category")
            Prelude.<*> (x Data..:? "code")
            Prelude.<*> (x Data..:? "service")
      )

instance Prelude.Hashable EventType where
  hashWithSalt _salt EventType' {..} =
    _salt `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` service

instance Prelude.NFData EventType where
  rnf EventType' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf service
