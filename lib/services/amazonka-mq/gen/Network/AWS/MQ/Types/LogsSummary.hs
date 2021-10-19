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
-- Module      : Network.AWS.MQ.Types.LogsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LogsSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.PendingLogs
import qualified Network.AWS.Prelude as Prelude

-- | The list of information about logs currently enabled and pending to be
-- deployed for the specified broker.
--
-- /See:/ 'newLogsSummary' smart constructor.
data LogsSummary = LogsSummary'
  { -- | The list of information about logs pending to be deployed for the
    -- specified broker.
    pending :: Prelude.Maybe PendingLogs,
    -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | The location of the CloudWatch Logs log group where audit logs are sent.
    auditLogGroup :: Prelude.Maybe Prelude.Text,
    -- | The location of the CloudWatch Logs log group where general logs are
    -- sent.
    generalLogGroup :: Prelude.Text,
    -- | Enables general logging.
    general :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pending', 'logsSummary_pending' - The list of information about logs pending to be deployed for the
-- specified broker.
--
-- 'audit', 'logsSummary_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
--
-- 'auditLogGroup', 'logsSummary_auditLogGroup' - The location of the CloudWatch Logs log group where audit logs are sent.
--
-- 'generalLogGroup', 'logsSummary_generalLogGroup' - The location of the CloudWatch Logs log group where general logs are
-- sent.
--
-- 'general', 'logsSummary_general' - Enables general logging.
newLogsSummary ::
  -- | 'generalLogGroup'
  Prelude.Text ->
  -- | 'general'
  Prelude.Bool ->
  LogsSummary
newLogsSummary pGeneralLogGroup_ pGeneral_ =
  LogsSummary'
    { pending = Prelude.Nothing,
      audit = Prelude.Nothing,
      auditLogGroup = Prelude.Nothing,
      generalLogGroup = pGeneralLogGroup_,
      general = pGeneral_
    }

-- | The list of information about logs pending to be deployed for the
-- specified broker.
logsSummary_pending :: Lens.Lens' LogsSummary (Prelude.Maybe PendingLogs)
logsSummary_pending = Lens.lens (\LogsSummary' {pending} -> pending) (\s@LogsSummary' {} a -> s {pending = a} :: LogsSummary)

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
logsSummary_audit :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Bool)
logsSummary_audit = Lens.lens (\LogsSummary' {audit} -> audit) (\s@LogsSummary' {} a -> s {audit = a} :: LogsSummary)

-- | The location of the CloudWatch Logs log group where audit logs are sent.
logsSummary_auditLogGroup :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Text)
logsSummary_auditLogGroup = Lens.lens (\LogsSummary' {auditLogGroup} -> auditLogGroup) (\s@LogsSummary' {} a -> s {auditLogGroup = a} :: LogsSummary)

-- | The location of the CloudWatch Logs log group where general logs are
-- sent.
logsSummary_generalLogGroup :: Lens.Lens' LogsSummary Prelude.Text
logsSummary_generalLogGroup = Lens.lens (\LogsSummary' {generalLogGroup} -> generalLogGroup) (\s@LogsSummary' {} a -> s {generalLogGroup = a} :: LogsSummary)

-- | Enables general logging.
logsSummary_general :: Lens.Lens' LogsSummary Prelude.Bool
logsSummary_general = Lens.lens (\LogsSummary' {general} -> general) (\s@LogsSummary' {} a -> s {general = a} :: LogsSummary)

instance Core.FromJSON LogsSummary where
  parseJSON =
    Core.withObject
      "LogsSummary"
      ( \x ->
          LogsSummary'
            Prelude.<$> (x Core..:? "pending")
            Prelude.<*> (x Core..:? "audit")
            Prelude.<*> (x Core..:? "auditLogGroup")
            Prelude.<*> (x Core..: "generalLogGroup")
            Prelude.<*> (x Core..: "general")
      )

instance Prelude.Hashable LogsSummary

instance Prelude.NFData LogsSummary
