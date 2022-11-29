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
-- Module      : Amazonka.Inspector.Types.Subscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types.EventSubscription
import qualified Amazonka.Prelude as Prelude

-- | This data type is used as a response element in the
-- ListEventSubscriptions action.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | The ARN of the assessment template that is used during the event for
    -- which the SNS notification is sent.
    resourceArn :: Prelude.Text,
    -- | The ARN of the Amazon Simple Notification Service (SNS) topic to which
    -- the SNS notifications are sent.
    topicArn :: Prelude.Text,
    -- | The list of existing event subscriptions.
    eventSubscriptions :: Prelude.NonEmpty EventSubscription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'subscription_resourceArn' - The ARN of the assessment template that is used during the event for
-- which the SNS notification is sent.
--
-- 'topicArn', 'subscription_topicArn' - The ARN of the Amazon Simple Notification Service (SNS) topic to which
-- the SNS notifications are sent.
--
-- 'eventSubscriptions', 'subscription_eventSubscriptions' - The list of existing event subscriptions.
newSubscription ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'topicArn'
  Prelude.Text ->
  -- | 'eventSubscriptions'
  Prelude.NonEmpty EventSubscription ->
  Subscription
newSubscription
  pResourceArn_
  pTopicArn_
  pEventSubscriptions_ =
    Subscription'
      { resourceArn = pResourceArn_,
        topicArn = pTopicArn_,
        eventSubscriptions =
          Lens.coerced Lens.# pEventSubscriptions_
      }

-- | The ARN of the assessment template that is used during the event for
-- which the SNS notification is sent.
subscription_resourceArn :: Lens.Lens' Subscription Prelude.Text
subscription_resourceArn = Lens.lens (\Subscription' {resourceArn} -> resourceArn) (\s@Subscription' {} a -> s {resourceArn = a} :: Subscription)

-- | The ARN of the Amazon Simple Notification Service (SNS) topic to which
-- the SNS notifications are sent.
subscription_topicArn :: Lens.Lens' Subscription Prelude.Text
subscription_topicArn = Lens.lens (\Subscription' {topicArn} -> topicArn) (\s@Subscription' {} a -> s {topicArn = a} :: Subscription)

-- | The list of existing event subscriptions.
subscription_eventSubscriptions :: Lens.Lens' Subscription (Prelude.NonEmpty EventSubscription)
subscription_eventSubscriptions = Lens.lens (\Subscription' {eventSubscriptions} -> eventSubscriptions) (\s@Subscription' {} a -> s {eventSubscriptions = a} :: Subscription) Prelude.. Lens.coerced

instance Core.FromJSON Subscription where
  parseJSON =
    Core.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Core..: "resourceArn")
            Prelude.<*> (x Core..: "topicArn")
            Prelude.<*> (x Core..: "eventSubscriptions")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` eventSubscriptions

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf eventSubscriptions
