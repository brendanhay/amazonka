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
-- Module      : Network.AWS.MQ.Types.LogsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LogsSummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.PendingLogs
import qualified Network.AWS.Prelude as Prelude

-- | The list of information about logs currently enabled and pending to be
-- deployed for the specified broker.
--
-- /See:/ 'newLogsSummary' smart constructor.
data LogsSummary = LogsSummary'
  { -- | Enables general logging.
    general :: Prelude.Maybe Prelude.Bool,
    -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | The list of information about logs pending to be deployed for the
    -- specified broker.
    pending :: Prelude.Maybe PendingLogs,
    -- | The location of the CloudWatch Logs log group where audit logs are sent.
    auditLogGroup :: Prelude.Maybe Prelude.Text,
    -- | The location of the CloudWatch Logs log group where general logs are
    -- sent.
    generalLogGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LogsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'general', 'logsSummary_general' - Enables general logging.
--
-- 'audit', 'logsSummary_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
--
-- 'pending', 'logsSummary_pending' - The list of information about logs pending to be deployed for the
-- specified broker.
--
-- 'auditLogGroup', 'logsSummary_auditLogGroup' - The location of the CloudWatch Logs log group where audit logs are sent.
--
-- 'generalLogGroup', 'logsSummary_generalLogGroup' - The location of the CloudWatch Logs log group where general logs are
-- sent.
newLogsSummary ::
  LogsSummary
newLogsSummary =
  LogsSummary'
    { general = Prelude.Nothing,
      audit = Prelude.Nothing,
      pending = Prelude.Nothing,
      auditLogGroup = Prelude.Nothing,
      generalLogGroup = Prelude.Nothing
    }

-- | Enables general logging.
logsSummary_general :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Bool)
logsSummary_general = Lens.lens (\LogsSummary' {general} -> general) (\s@LogsSummary' {} a -> s {general = a} :: LogsSummary)

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
logsSummary_audit :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Bool)
logsSummary_audit = Lens.lens (\LogsSummary' {audit} -> audit) (\s@LogsSummary' {} a -> s {audit = a} :: LogsSummary)

-- | The list of information about logs pending to be deployed for the
-- specified broker.
logsSummary_pending :: Lens.Lens' LogsSummary (Prelude.Maybe PendingLogs)
logsSummary_pending = Lens.lens (\LogsSummary' {pending} -> pending) (\s@LogsSummary' {} a -> s {pending = a} :: LogsSummary)

-- | The location of the CloudWatch Logs log group where audit logs are sent.
logsSummary_auditLogGroup :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Text)
logsSummary_auditLogGroup = Lens.lens (\LogsSummary' {auditLogGroup} -> auditLogGroup) (\s@LogsSummary' {} a -> s {auditLogGroup = a} :: LogsSummary)

-- | The location of the CloudWatch Logs log group where general logs are
-- sent.
logsSummary_generalLogGroup :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Text)
logsSummary_generalLogGroup = Lens.lens (\LogsSummary' {generalLogGroup} -> generalLogGroup) (\s@LogsSummary' {} a -> s {generalLogGroup = a} :: LogsSummary)

instance Prelude.FromJSON LogsSummary where
  parseJSON =
    Prelude.withObject
      "LogsSummary"
      ( \x ->
          LogsSummary'
            Prelude.<$> (x Prelude..:? "general")
            Prelude.<*> (x Prelude..:? "audit")
            Prelude.<*> (x Prelude..:? "pending")
            Prelude.<*> (x Prelude..:? "auditLogGroup")
            Prelude.<*> (x Prelude..:? "generalLogGroup")
      )

instance Prelude.Hashable LogsSummary

instance Prelude.NFData LogsSummary
