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
-- Module      : Amazonka.Greengrass.Types.Subscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.Subscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a subscription.
--
-- /See:/ 'newSubscription' smart constructor.
data Subscription = Subscription'
  { -- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN,
    -- a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
    -- \'GGShadowService\'.
    target :: Prelude.Text,
    -- | A descriptive or arbitrary ID for the subscription. This value must be
    -- unique within the subscription definition version. Max length is 128
    -- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text,
    -- | The MQTT topic used to route the message.
    subject :: Prelude.Text,
    -- | The source of the subscription. Can be a thing ARN, a Lambda function
    -- ARN, a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
    -- \'GGShadowService\'.
    source :: Prelude.Text
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
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'subject'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
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
subscription_target :: Lens.Lens' Subscription Prelude.Text
subscription_target = Lens.lens (\Subscription' {target} -> target) (\s@Subscription' {} a -> s {target = a} :: Subscription)

-- | A descriptive or arbitrary ID for the subscription. This value must be
-- unique within the subscription definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
subscription_id :: Lens.Lens' Subscription Prelude.Text
subscription_id = Lens.lens (\Subscription' {id} -> id) (\s@Subscription' {} a -> s {id = a} :: Subscription)

-- | The MQTT topic used to route the message.
subscription_subject :: Lens.Lens' Subscription Prelude.Text
subscription_subject = Lens.lens (\Subscription' {subject} -> subject) (\s@Subscription' {} a -> s {subject = a} :: Subscription)

-- | The source of the subscription. Can be a thing ARN, a Lambda function
-- ARN, a connector ARN, \'cloud\' (which represents the AWS IoT cloud), or
-- \'GGShadowService\'.
subscription_source :: Lens.Lens' Subscription Prelude.Text
subscription_source = Lens.lens (\Subscription' {source} -> source) (\s@Subscription' {} a -> s {source = a} :: Subscription)

instance Data.FromJSON Subscription where
  parseJSON =
    Data.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Prelude.<$> (x Data..: "Target")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Subject")
            Prelude.<*> (x Data..: "Source")
      )

instance Prelude.Hashable Subscription where
  hashWithSalt _salt Subscription' {..} =
    _salt `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` source

instance Prelude.NFData Subscription where
  rnf Subscription' {..} =
    Prelude.rnf target
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf source

instance Data.ToJSON Subscription where
  toJSON Subscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Target" Data..= target),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Subject" Data..= subject),
            Prelude.Just ("Source" Data..= source)
          ]
      )
