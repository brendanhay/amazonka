{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.TopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestination where

import Network.AWS.IoT.Types.HttpUrlDestinationProperties
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import Network.AWS.IoT.Types.VpcDestinationProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A topic rule destination.
--
-- /See:/ 'newTopicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { -- | The status of the topic rule destination. Valid values are:
    --
    -- [IN_PROGRESS]
    --     A topic rule destination was created but has not been confirmed. You
    --     can set @status@ to @IN_PROGRESS@ by calling
    --     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
    --     causes a new confirmation challenge to be sent to your confirmation
    --     endpoint.
    --
    -- [ENABLED]
    --     Confirmation was completed, and traffic to this destination is
    --     allowed. You can set @status@ to @DISABLED@ by calling
    --     @UpdateTopicRuleDestination@.
    --
    -- [DISABLED]
    --     Confirmation was completed, and traffic to this destination is not
    --     allowed. You can set @status@ to @ENABLED@ by calling
    --     @UpdateTopicRuleDestination@.
    --
    -- [ERROR]
    --     Confirmation could not be completed, for example if the confirmation
    --     timed out. You can call @GetTopicRuleDestination@ for details about
    --     the error. You can set @status@ to @IN_PROGRESS@ by calling
    --     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
    --     causes a new confirmation challenge to be sent to your confirmation
    --     endpoint.
    status :: Prelude.Maybe TopicRuleDestinationStatus,
    -- | The date and time when the topic rule destination was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The topic rule destination URL.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Properties of the virtual private cloud (VPC) connection.
    vpcProperties :: Prelude.Maybe VpcDestinationProperties,
    -- | Additional details or reason why the topic rule destination is in the
    -- current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Properties of the HTTP URL.
    httpUrlProperties :: Prelude.Maybe HttpUrlDestinationProperties,
    -- | The date and time when the topic rule destination was last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'topicRuleDestination_status' - The status of the topic rule destination. Valid values are:
--
-- [IN_PROGRESS]
--     A topic rule destination was created but has not been confirmed. You
--     can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
--
-- [ENABLED]
--     Confirmation was completed, and traffic to this destination is
--     allowed. You can set @status@ to @DISABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [DISABLED]
--     Confirmation was completed, and traffic to this destination is not
--     allowed. You can set @status@ to @ENABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [ERROR]
--     Confirmation could not be completed, for example if the confirmation
--     timed out. You can call @GetTopicRuleDestination@ for details about
--     the error. You can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
--
-- 'createdAt', 'topicRuleDestination_createdAt' - The date and time when the topic rule destination was created.
--
-- 'arn', 'topicRuleDestination_arn' - The topic rule destination URL.
--
-- 'vpcProperties', 'topicRuleDestination_vpcProperties' - Properties of the virtual private cloud (VPC) connection.
--
-- 'statusReason', 'topicRuleDestination_statusReason' - Additional details or reason why the topic rule destination is in the
-- current status.
--
-- 'httpUrlProperties', 'topicRuleDestination_httpUrlProperties' - Properties of the HTTP URL.
--
-- 'lastUpdatedAt', 'topicRuleDestination_lastUpdatedAt' - The date and time when the topic rule destination was last updated.
newTopicRuleDestination ::
  TopicRuleDestination
newTopicRuleDestination =
  TopicRuleDestination'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      vpcProperties = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      httpUrlProperties = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
    }

-- | The status of the topic rule destination. Valid values are:
--
-- [IN_PROGRESS]
--     A topic rule destination was created but has not been confirmed. You
--     can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
--
-- [ENABLED]
--     Confirmation was completed, and traffic to this destination is
--     allowed. You can set @status@ to @DISABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [DISABLED]
--     Confirmation was completed, and traffic to this destination is not
--     allowed. You can set @status@ to @ENABLED@ by calling
--     @UpdateTopicRuleDestination@.
--
-- [ERROR]
--     Confirmation could not be completed, for example if the confirmation
--     timed out. You can call @GetTopicRuleDestination@ for details about
--     the error. You can set @status@ to @IN_PROGRESS@ by calling
--     @UpdateTopicRuleDestination@. Calling @UpdateTopicRuleDestination@
--     causes a new confirmation challenge to be sent to your confirmation
--     endpoint.
topicRuleDestination_status :: Lens.Lens' TopicRuleDestination (Prelude.Maybe TopicRuleDestinationStatus)
topicRuleDestination_status = Lens.lens (\TopicRuleDestination' {status} -> status) (\s@TopicRuleDestination' {} a -> s {status = a} :: TopicRuleDestination)

-- | The date and time when the topic rule destination was created.
topicRuleDestination_createdAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_createdAt = Lens.lens (\TopicRuleDestination' {createdAt} -> createdAt) (\s@TopicRuleDestination' {} a -> s {createdAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Prelude._Time

-- | The topic rule destination URL.
topicRuleDestination_arn :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_arn = Lens.lens (\TopicRuleDestination' {arn} -> arn) (\s@TopicRuleDestination' {} a -> s {arn = a} :: TopicRuleDestination)

-- | Properties of the virtual private cloud (VPC) connection.
topicRuleDestination_vpcProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe VpcDestinationProperties)
topicRuleDestination_vpcProperties = Lens.lens (\TopicRuleDestination' {vpcProperties} -> vpcProperties) (\s@TopicRuleDestination' {} a -> s {vpcProperties = a} :: TopicRuleDestination)

-- | Additional details or reason why the topic rule destination is in the
-- current status.
topicRuleDestination_statusReason :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_statusReason = Lens.lens (\TopicRuleDestination' {statusReason} -> statusReason) (\s@TopicRuleDestination' {} a -> s {statusReason = a} :: TopicRuleDestination)

-- | Properties of the HTTP URL.
topicRuleDestination_httpUrlProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe HttpUrlDestinationProperties)
topicRuleDestination_httpUrlProperties = Lens.lens (\TopicRuleDestination' {httpUrlProperties} -> httpUrlProperties) (\s@TopicRuleDestination' {} a -> s {httpUrlProperties = a} :: TopicRuleDestination)

-- | The date and time when the topic rule destination was last updated.
topicRuleDestination_lastUpdatedAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_lastUpdatedAt = Lens.lens (\TopicRuleDestination' {lastUpdatedAt} -> lastUpdatedAt) (\s@TopicRuleDestination' {} a -> s {lastUpdatedAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TopicRuleDestination where
  parseJSON =
    Prelude.withObject
      "TopicRuleDestination"
      ( \x ->
          TopicRuleDestination'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "vpcProperties")
            Prelude.<*> (x Prelude..:? "statusReason")
            Prelude.<*> (x Prelude..:? "httpUrlProperties")
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
      )

instance Prelude.Hashable TopicRuleDestination

instance Prelude.NFData TopicRuleDestination
