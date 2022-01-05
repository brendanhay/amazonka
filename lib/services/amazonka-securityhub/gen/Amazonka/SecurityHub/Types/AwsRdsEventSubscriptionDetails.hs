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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsEventSubscriptionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsEventSubscriptionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon RDS event notification subscription. The
-- subscription allows Amazon RDS to post events to an SNS topic.
--
-- /See:/ 'newAwsRdsEventSubscriptionDetails' smart constructor.
data AwsRdsEventSubscriptionDetails = AwsRdsEventSubscriptionDetails'
  { -- | The status of the event notification subscription.
    --
    -- Valid values: @creating@ | @modifying@ | @deleting@ | @active@ |
    -- @no-permission@ | @topic-not-exist@
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the event notification subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the account that is associated with the event
    -- notification subscription.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the SNS topic to post the event notifications to.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event notification subscription.
    eventSubscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the event notification subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The source type for the event notification subscription.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The datetime when the event notification subscription was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    subscriptionCreationTime :: Prelude.Maybe Prelude.Text,
    -- | The list of event categories for the event notification subscription.
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | A list of source identifiers for the event notification subscription.
    sourceIdsList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsEventSubscriptionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsEventSubscriptionDetails_status' - The status of the event notification subscription.
--
-- Valid values: @creating@ | @modifying@ | @deleting@ | @active@ |
-- @no-permission@ | @topic-not-exist@
--
-- 'customerAwsId', 'awsRdsEventSubscriptionDetails_customerAwsId' - The identifier of the event notification subscription.
--
-- 'custSubscriptionId', 'awsRdsEventSubscriptionDetails_custSubscriptionId' - The identifier of the account that is associated with the event
-- notification subscription.
--
-- 'snsTopicArn', 'awsRdsEventSubscriptionDetails_snsTopicArn' - The ARN of the SNS topic to post the event notifications to.
--
-- 'eventSubscriptionArn', 'awsRdsEventSubscriptionDetails_eventSubscriptionArn' - The ARN of the event notification subscription.
--
-- 'enabled', 'awsRdsEventSubscriptionDetails_enabled' - Whether the event notification subscription is enabled.
--
-- 'sourceType', 'awsRdsEventSubscriptionDetails_sourceType' - The source type for the event notification subscription.
--
-- 'subscriptionCreationTime', 'awsRdsEventSubscriptionDetails_subscriptionCreationTime' - The datetime when the event notification subscription was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'eventCategoriesList', 'awsRdsEventSubscriptionDetails_eventCategoriesList' - The list of event categories for the event notification subscription.
--
-- 'sourceIdsList', 'awsRdsEventSubscriptionDetails_sourceIdsList' - A list of source identifiers for the event notification subscription.
newAwsRdsEventSubscriptionDetails ::
  AwsRdsEventSubscriptionDetails
newAwsRdsEventSubscriptionDetails =
  AwsRdsEventSubscriptionDetails'
    { status =
        Prelude.Nothing,
      customerAwsId = Prelude.Nothing,
      custSubscriptionId = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      eventSubscriptionArn = Prelude.Nothing,
      enabled = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      subscriptionCreationTime = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing
    }

-- | The status of the event notification subscription.
--
-- Valid values: @creating@ | @modifying@ | @deleting@ | @active@ |
-- @no-permission@ | @topic-not-exist@
awsRdsEventSubscriptionDetails_status :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_status = Lens.lens (\AwsRdsEventSubscriptionDetails' {status} -> status) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {status = a} :: AwsRdsEventSubscriptionDetails)

-- | The identifier of the event notification subscription.
awsRdsEventSubscriptionDetails_customerAwsId :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_customerAwsId = Lens.lens (\AwsRdsEventSubscriptionDetails' {customerAwsId} -> customerAwsId) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {customerAwsId = a} :: AwsRdsEventSubscriptionDetails)

-- | The identifier of the account that is associated with the event
-- notification subscription.
awsRdsEventSubscriptionDetails_custSubscriptionId :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_custSubscriptionId = Lens.lens (\AwsRdsEventSubscriptionDetails' {custSubscriptionId} -> custSubscriptionId) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {custSubscriptionId = a} :: AwsRdsEventSubscriptionDetails)

-- | The ARN of the SNS topic to post the event notifications to.
awsRdsEventSubscriptionDetails_snsTopicArn :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_snsTopicArn = Lens.lens (\AwsRdsEventSubscriptionDetails' {snsTopicArn} -> snsTopicArn) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {snsTopicArn = a} :: AwsRdsEventSubscriptionDetails)

-- | The ARN of the event notification subscription.
awsRdsEventSubscriptionDetails_eventSubscriptionArn :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_eventSubscriptionArn = Lens.lens (\AwsRdsEventSubscriptionDetails' {eventSubscriptionArn} -> eventSubscriptionArn) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {eventSubscriptionArn = a} :: AwsRdsEventSubscriptionDetails)

-- | Whether the event notification subscription is enabled.
awsRdsEventSubscriptionDetails_enabled :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Bool)
awsRdsEventSubscriptionDetails_enabled = Lens.lens (\AwsRdsEventSubscriptionDetails' {enabled} -> enabled) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {enabled = a} :: AwsRdsEventSubscriptionDetails)

-- | The source type for the event notification subscription.
awsRdsEventSubscriptionDetails_sourceType :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_sourceType = Lens.lens (\AwsRdsEventSubscriptionDetails' {sourceType} -> sourceType) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {sourceType = a} :: AwsRdsEventSubscriptionDetails)

-- | The datetime when the event notification subscription was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsEventSubscriptionDetails_subscriptionCreationTime :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe Prelude.Text)
awsRdsEventSubscriptionDetails_subscriptionCreationTime = Lens.lens (\AwsRdsEventSubscriptionDetails' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {subscriptionCreationTime = a} :: AwsRdsEventSubscriptionDetails)

-- | The list of event categories for the event notification subscription.
awsRdsEventSubscriptionDetails_eventCategoriesList :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe [Prelude.Text])
awsRdsEventSubscriptionDetails_eventCategoriesList = Lens.lens (\AwsRdsEventSubscriptionDetails' {eventCategoriesList} -> eventCategoriesList) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {eventCategoriesList = a} :: AwsRdsEventSubscriptionDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of source identifiers for the event notification subscription.
awsRdsEventSubscriptionDetails_sourceIdsList :: Lens.Lens' AwsRdsEventSubscriptionDetails (Prelude.Maybe [Prelude.Text])
awsRdsEventSubscriptionDetails_sourceIdsList = Lens.lens (\AwsRdsEventSubscriptionDetails' {sourceIdsList} -> sourceIdsList) (\s@AwsRdsEventSubscriptionDetails' {} a -> s {sourceIdsList = a} :: AwsRdsEventSubscriptionDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsRdsEventSubscriptionDetails where
  parseJSON =
    Core.withObject
      "AwsRdsEventSubscriptionDetails"
      ( \x ->
          AwsRdsEventSubscriptionDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CustomerAwsId")
            Prelude.<*> (x Core..:? "CustSubscriptionId")
            Prelude.<*> (x Core..:? "SnsTopicArn")
            Prelude.<*> (x Core..:? "EventSubscriptionArn")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SubscriptionCreationTime")
            Prelude.<*> ( x Core..:? "EventCategoriesList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceIdsList" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsRdsEventSubscriptionDetails
  where
  hashWithSalt
    _salt
    AwsRdsEventSubscriptionDetails' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` customerAwsId
        `Prelude.hashWithSalt` custSubscriptionId
        `Prelude.hashWithSalt` snsTopicArn
        `Prelude.hashWithSalt` eventSubscriptionArn
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` sourceType
        `Prelude.hashWithSalt` subscriptionCreationTime
        `Prelude.hashWithSalt` eventCategoriesList
        `Prelude.hashWithSalt` sourceIdsList

instance
  Prelude.NFData
    AwsRdsEventSubscriptionDetails
  where
  rnf AwsRdsEventSubscriptionDetails' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf customerAwsId
      `Prelude.seq` Prelude.rnf custSubscriptionId
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf eventSubscriptionArn
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf subscriptionCreationTime
      `Prelude.seq` Prelude.rnf eventCategoriesList
      `Prelude.seq` Prelude.rnf sourceIdsList

instance Core.ToJSON AwsRdsEventSubscriptionDetails where
  toJSON AwsRdsEventSubscriptionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("CustomerAwsId" Core..=) Prelude.<$> customerAwsId,
            ("CustSubscriptionId" Core..=)
              Prelude.<$> custSubscriptionId,
            ("SnsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("EventSubscriptionArn" Core..=)
              Prelude.<$> eventSubscriptionArn,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("SourceType" Core..=) Prelude.<$> sourceType,
            ("SubscriptionCreationTime" Core..=)
              Prelude.<$> subscriptionCreationTime,
            ("EventCategoriesList" Core..=)
              Prelude.<$> eventCategoriesList,
            ("SourceIdsList" Core..=) Prelude.<$> sourceIdsList
          ]
      )
