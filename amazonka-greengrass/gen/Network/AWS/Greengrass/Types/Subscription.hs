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
-- Module      : Network.AWS.Greengrass.Types.Subscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Subscription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN,
    -- a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
    -- \'GGShadowService\'.
    target :: Core.Text,
    -- | A descriptive or arbitrary ID for the subscription. This value must be
    -- unique within the subscription definition version. Max length is 128
    -- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Core.Text,
    -- | The MQTT topic used to route the message.
    subject :: Core.Text,
    -- | The source of the subscription. Can be a thing ARN, a Lambda function
    -- ARN, a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
    -- \'GGShadowService\'.
    source :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Subscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'subscription_target' - Where the message is sent to. Can be a thing ARN, a Lambda function ARN,
-- a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
-- \'GGShadowService\'.
--
-- 'id', 'subscription_id' - A descriptive or arbitrary ID for the subscription. This value must be
-- unique within the subscription definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
--
-- 'subject', 'subscription_subject' - The MQTT topic used to route the message.
--
-- 'source', 'subscription_source' - The source of the subscription. Can be a thing ARN, a Lambda function
-- ARN, a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
-- \'GGShadowService\'.
newSubscription ::
  -- | 'target'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  -- | 'subject'
  Core.Text ->
  -- | 'source'
  Core.Text ->
  Subscription
newSubscription pTarget_ pId_ pSubject_ pSource_ =
  Subscription'
    { target = pTarget_,
      id = pId_,
      subject = pSubject_,
      source = pSource_
    }

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN,
-- a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
-- \'GGShadowService\'.
subscription_target :: Lens.Lens' Subscription Core.Text
subscription_target = Lens.lens (\Subscription' {target} -> target) (\s@Subscription' {} a -> s {target = a} :: Subscription)

-- | A descriptive or arbitrary ID for the subscription. This value must be
-- unique within the subscription definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
subscription_id :: Lens.Lens' Subscription Core.Text
subscription_id = Lens.lens (\Subscription' {id} -> id) (\s@Subscription' {} a -> s {id = a} :: Subscription)

-- | The MQTT topic used to route the message.
subscription_subject :: Lens.Lens' Subscription Core.Text
subscription_subject = Lens.lens (\Subscription' {subject} -> subject) (\s@Subscription' {} a -> s {subject = a} :: Subscription)

-- | The source of the subscription. Can be a thing ARN, a Lambda function
-- ARN, a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
-- \'GGShadowService\'.
subscription_source :: Lens.Lens' Subscription Core.Text
subscription_source = Lens.lens (\Subscription' {source} -> source) (\s@Subscription' {} a -> s {source = a} :: Subscription)

instance Core.FromJSON Subscription where
  parseJSON =
    Core.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Core.<$> (x Core..: "Target")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "Subject")
            Core.<*> (x Core..: "Source")
      )

instance Core.Hashable Subscription

instance Core.NFData Subscription

instance Core.ToJSON Subscription where
  toJSON Subscription' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Target" Core..= target),
            Core.Just ("Id" Core..= id),
            Core.Just ("Subject" Core..= subject),
            Core.Just ("Source" Core..= source)
          ]
      )
