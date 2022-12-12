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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRuleDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.HttpUrlDestinationProperties
import Amazonka.IoT.Types.TopicRuleDestinationStatus
import Amazonka.IoT.Types.VpcDestinationProperties
import qualified Amazonka.Prelude as Prelude

-- | A topic rule destination.
--
-- /See:/ 'newTopicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { -- | The topic rule destination URL.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the topic rule destination was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Properties of the HTTP URL.
    httpUrlProperties :: Prelude.Maybe HttpUrlDestinationProperties,
    -- | The date and time when the topic rule destination was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
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
    -- | Additional details or reason why the topic rule destination is in the
    -- current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Properties of the virtual private cloud (VPC) connection.
    vpcProperties :: Prelude.Maybe VpcDestinationProperties
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
-- 'arn', 'topicRuleDestination_arn' - The topic rule destination URL.
--
-- 'createdAt', 'topicRuleDestination_createdAt' - The date and time when the topic rule destination was created.
--
-- 'httpUrlProperties', 'topicRuleDestination_httpUrlProperties' - Properties of the HTTP URL.
--
-- 'lastUpdatedAt', 'topicRuleDestination_lastUpdatedAt' - The date and time when the topic rule destination was last updated.
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
-- 'statusReason', 'topicRuleDestination_statusReason' - Additional details or reason why the topic rule destination is in the
-- current status.
--
-- 'vpcProperties', 'topicRuleDestination_vpcProperties' - Properties of the virtual private cloud (VPC) connection.
newTopicRuleDestination ::
  TopicRuleDestination
newTopicRuleDestination =
  TopicRuleDestination'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpUrlProperties = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      vpcProperties = Prelude.Nothing
    }

-- | The topic rule destination URL.
topicRuleDestination_arn :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_arn = Lens.lens (\TopicRuleDestination' {arn} -> arn) (\s@TopicRuleDestination' {} a -> s {arn = a} :: TopicRuleDestination)

-- | The date and time when the topic rule destination was created.
topicRuleDestination_createdAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_createdAt = Lens.lens (\TopicRuleDestination' {createdAt} -> createdAt) (\s@TopicRuleDestination' {} a -> s {createdAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Data._Time

-- | Properties of the HTTP URL.
topicRuleDestination_httpUrlProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe HttpUrlDestinationProperties)
topicRuleDestination_httpUrlProperties = Lens.lens (\TopicRuleDestination' {httpUrlProperties} -> httpUrlProperties) (\s@TopicRuleDestination' {} a -> s {httpUrlProperties = a} :: TopicRuleDestination)

-- | The date and time when the topic rule destination was last updated.
topicRuleDestination_lastUpdatedAt :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.UTCTime)
topicRuleDestination_lastUpdatedAt = Lens.lens (\TopicRuleDestination' {lastUpdatedAt} -> lastUpdatedAt) (\s@TopicRuleDestination' {} a -> s {lastUpdatedAt = a} :: TopicRuleDestination) Prelude.. Lens.mapping Data._Time

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

-- | Additional details or reason why the topic rule destination is in the
-- current status.
topicRuleDestination_statusReason :: Lens.Lens' TopicRuleDestination (Prelude.Maybe Prelude.Text)
topicRuleDestination_statusReason = Lens.lens (\TopicRuleDestination' {statusReason} -> statusReason) (\s@TopicRuleDestination' {} a -> s {statusReason = a} :: TopicRuleDestination)

-- | Properties of the virtual private cloud (VPC) connection.
topicRuleDestination_vpcProperties :: Lens.Lens' TopicRuleDestination (Prelude.Maybe VpcDestinationProperties)
topicRuleDestination_vpcProperties = Lens.lens (\TopicRuleDestination' {vpcProperties} -> vpcProperties) (\s@TopicRuleDestination' {} a -> s {vpcProperties = a} :: TopicRuleDestination)

instance Data.FromJSON TopicRuleDestination where
  parseJSON =
    Data.withObject
      "TopicRuleDestination"
      ( \x ->
          TopicRuleDestination'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "httpUrlProperties")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "vpcProperties")
      )

instance Prelude.Hashable TopicRuleDestination where
  hashWithSalt _salt TopicRuleDestination' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` httpUrlProperties
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` vpcProperties

instance Prelude.NFData TopicRuleDestination where
  rnf TopicRuleDestination' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpUrlProperties
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf vpcProperties
