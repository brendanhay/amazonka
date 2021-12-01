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
-- Module      : Amazonka.IoT.Types.TopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRuleDestination where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.HttpUrlDestinationProperties
import Amazonka.IoT.Types.TopicRuleDestinationStatus
import Amazonka.IoT.Types.VpcDestinationProperties
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A topic rule destination.
--
-- /See:/ 'newTopicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { -- | Properties of the virtual private cloud (VPC) connection.
    vpcProperties :: Prelude.Maybe VpcDestinationProperties,
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
    status :: Prelude.Maybe TopicRuleDestinationStatus,
    -- | The date and time when the topic rule destination was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | Properties of the HTTP URL.
    httpUrlProperties :: Prelude.Maybe HttpUrlDestinationProperties,
    -- | The topic rule destination URL.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the topic rule destination was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Additional details or reason why the topic rule destination is in the
    -- current status.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcProperties', 'topicRuleDestination_vpcProperties' - Properties of the virtual private cloud (VPC) connection.
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
-- 'lastUpdatedAt', 'topicRuleDestination_lastUpdatedAt' - The date and time when the topic rule destination was last updated.
--
-- 'httpUrlProperties', 'topicRuleDestination_httpUrlProperties' - Properties of the HTTP URL.
--
-- 'arn', 'topicRuleDestination_arn' - The topic rule destination URL.
--
-- 'createdAt', 'topicRuleDestination_createdAt' - The date and time when the topic rule destination was created.
--
-- 'statusReason', 'topicRuleDestination_statusReason' - Additional details or reason why the topic rule destination is in the
-- current status.
newTopicRuleDestination ::
  TopicRuleDestination
newTopicRuleDestination =
  TopicRuleDestination'
    { vpcProperties =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      httpUrlProperties = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | Properties of the virtual private cloud (VPC) connection.
topicRuleDestination_vpcProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe VpcDestinationProperties)
topicRuleDestination_vpcProperties = Lens.lens (\TopicRuleDestination' {vpcProperties} -> vpcProperties) (\s@TopicRuleDestination' {} a -> s {vpcProperties = a} :: TopicRuleDestination)

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

-- | The date and time when the topic rule destination was last updated.
topicRuleDestination_lastUpdatedAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_lastUpdatedAt = Lens.lens (\TopicRuleDestination' {lastUpdatedAt} -> lastUpdatedAt) (\s@TopicRuleDestination' {} a -> s {lastUpdatedAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Core._Time

-- | Properties of the HTTP URL.
topicRuleDestination_httpUrlProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe HttpUrlDestinationProperties)
topicRuleDestination_httpUrlProperties = Lens.lens (\TopicRuleDestination' {httpUrlProperties} -> httpUrlProperties) (\s@TopicRuleDestination' {} a -> s {httpUrlProperties = a} :: TopicRuleDestination)

-- | The topic rule destination URL.
topicRuleDestination_arn :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_arn = Lens.lens (\TopicRuleDestination' {arn} -> arn) (\s@TopicRuleDestination' {} a -> s {arn = a} :: TopicRuleDestination)

-- | The date and time when the topic rule destination was created.
topicRuleDestination_createdAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_createdAt = Lens.lens (\TopicRuleDestination' {createdAt} -> createdAt) (\s@TopicRuleDestination' {} a -> s {createdAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Core._Time

-- | Additional details or reason why the topic rule destination is in the
-- current status.
topicRuleDestination_statusReason :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_statusReason = Lens.lens (\TopicRuleDestination' {statusReason} -> statusReason) (\s@TopicRuleDestination' {} a -> s {statusReason = a} :: TopicRuleDestination)

instance Core.FromJSON TopicRuleDestination where
  parseJSON =
    Core.withObject
      "TopicRuleDestination"
      ( \x ->
          TopicRuleDestination'
            Prelude.<$> (x Core..:? "vpcProperties")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "httpUrlProperties")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "statusReason")
      )

instance Prelude.Hashable TopicRuleDestination where
  hashWithSalt salt' TopicRuleDestination' {..} =
    salt' `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` httpUrlProperties
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vpcProperties

instance Prelude.NFData TopicRuleDestination where
  rnf TopicRuleDestination' {..} =
    Prelude.rnf vpcProperties
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpUrlProperties
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf status
