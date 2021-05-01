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
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationSummary where

import Network.AWS.IoT.Types.HttpUrlDestinationSummary
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import Network.AWS.IoT.Types.VpcDestinationSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the topic rule destination.
--
-- /See:/ 'newTopicRuleDestinationSummary' smart constructor.
data TopicRuleDestinationSummary = TopicRuleDestinationSummary'
  { -- | Information about the HTTP URL.
    httpUrlSummary :: Prelude.Maybe HttpUrlDestinationSummary,
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
    -- | The date and time when the topic rule destination was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The topic rule destination ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The reason the topic rule destination is in the current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Information about the virtual private cloud (VPC) connection.
    vpcDestinationSummary :: Prelude.Maybe VpcDestinationSummary,
    -- | The date and time when the topic rule destination was last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TopicRuleDestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpUrlSummary', 'topicRuleDestinationSummary_httpUrlSummary' - Information about the HTTP URL.
--
-- 'status', 'topicRuleDestinationSummary_status' - The status of the topic rule destination. Valid values are:
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
-- 'createdAt', 'topicRuleDestinationSummary_createdAt' - The date and time when the topic rule destination was created.
--
-- 'arn', 'topicRuleDestinationSummary_arn' - The topic rule destination ARN.
--
-- 'statusReason', 'topicRuleDestinationSummary_statusReason' - The reason the topic rule destination is in the current status.
--
-- 'vpcDestinationSummary', 'topicRuleDestinationSummary_vpcDestinationSummary' - Information about the virtual private cloud (VPC) connection.
--
-- 'lastUpdatedAt', 'topicRuleDestinationSummary_lastUpdatedAt' - The date and time when the topic rule destination was last updated.
newTopicRuleDestinationSummary ::
  TopicRuleDestinationSummary
newTopicRuleDestinationSummary =
  TopicRuleDestinationSummary'
    { httpUrlSummary =
        Prelude.Nothing,
      status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      vpcDestinationSummary = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing
    }

-- | Information about the HTTP URL.
topicRuleDestinationSummary_httpUrlSummary :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe HttpUrlDestinationSummary)
topicRuleDestinationSummary_httpUrlSummary = Lens.lens (\TopicRuleDestinationSummary' {httpUrlSummary} -> httpUrlSummary) (\s@TopicRuleDestinationSummary' {} a -> s {httpUrlSummary = a} :: TopicRuleDestinationSummary)

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
topicRuleDestinationSummary_status :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe TopicRuleDestinationStatus)
topicRuleDestinationSummary_status = Lens.lens (\TopicRuleDestinationSummary' {status} -> status) (\s@TopicRuleDestinationSummary' {} a -> s {status = a} :: TopicRuleDestinationSummary)

-- | The date and time when the topic rule destination was created.
topicRuleDestinationSummary_createdAt :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe Prelude.UTCTime)
topicRuleDestinationSummary_createdAt = Lens.lens (\TopicRuleDestinationSummary' {createdAt} -> createdAt) (\s@TopicRuleDestinationSummary' {} a -> s {createdAt = a} :: TopicRuleDestinationSummary) Prelude.. Lens.mapping Prelude._Time

-- | The topic rule destination ARN.
topicRuleDestinationSummary_arn :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe Prelude.Text)
topicRuleDestinationSummary_arn = Lens.lens (\TopicRuleDestinationSummary' {arn} -> arn) (\s@TopicRuleDestinationSummary' {} a -> s {arn = a} :: TopicRuleDestinationSummary)

-- | The reason the topic rule destination is in the current status.
topicRuleDestinationSummary_statusReason :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe Prelude.Text)
topicRuleDestinationSummary_statusReason = Lens.lens (\TopicRuleDestinationSummary' {statusReason} -> statusReason) (\s@TopicRuleDestinationSummary' {} a -> s {statusReason = a} :: TopicRuleDestinationSummary)

-- | Information about the virtual private cloud (VPC) connection.
topicRuleDestinationSummary_vpcDestinationSummary :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe VpcDestinationSummary)
topicRuleDestinationSummary_vpcDestinationSummary = Lens.lens (\TopicRuleDestinationSummary' {vpcDestinationSummary} -> vpcDestinationSummary) (\s@TopicRuleDestinationSummary' {} a -> s {vpcDestinationSummary = a} :: TopicRuleDestinationSummary)

-- | The date and time when the topic rule destination was last updated.
topicRuleDestinationSummary_lastUpdatedAt :: Lens.Lens' TopicRuleDestinationSummary (Prelude.Maybe Prelude.UTCTime)
topicRuleDestinationSummary_lastUpdatedAt = Lens.lens (\TopicRuleDestinationSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@TopicRuleDestinationSummary' {} a -> s {lastUpdatedAt = a} :: TopicRuleDestinationSummary) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TopicRuleDestinationSummary where
  parseJSON =
    Prelude.withObject
      "TopicRuleDestinationSummary"
      ( \x ->
          TopicRuleDestinationSummary'
            Prelude.<$> (x Prelude..:? "httpUrlSummary")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "statusReason")
            Prelude.<*> (x Prelude..:? "vpcDestinationSummary")
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
      )

instance Prelude.Hashable TopicRuleDestinationSummary

instance Prelude.NFData TopicRuleDestinationSummary
