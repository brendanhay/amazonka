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
-- Module      : Amazonka.MQ.Types.LogsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.LogsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.PendingLogs
import qualified Amazonka.Prelude as Prelude

-- | The list of information about logs currently enabled and pending to be
-- deployed for the specified broker.
--
-- /See:/ 'newLogsSummary' smart constructor.
data LogsSummary = LogsSummary'
  { -- | The location of the CloudWatch Logs log group where audit logs are sent.
    auditLogGroup :: Prelude.Maybe Prelude.Text,
    -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | The list of information about logs pending to be deployed for the
    -- specified broker.
    pending :: Prelude.Maybe PendingLogs,
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
-- 'auditLogGroup', 'logsSummary_auditLogGroup' - The location of the CloudWatch Logs log group where audit logs are sent.
--
-- 'audit', 'logsSummary_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
--
-- 'pending', 'logsSummary_pending' - The list of information about logs pending to be deployed for the
-- specified broker.
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
    { auditLogGroup = Prelude.Nothing,
      audit = Prelude.Nothing,
      pending = Prelude.Nothing,
      generalLogGroup = pGeneralLogGroup_,
      general = pGeneral_
    }

-- | The location of the CloudWatch Logs log group where audit logs are sent.
logsSummary_auditLogGroup :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Text)
logsSummary_auditLogGroup = Lens.lens (\LogsSummary' {auditLogGroup} -> auditLogGroup) (\s@LogsSummary' {} a -> s {auditLogGroup = a} :: LogsSummary)

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
logsSummary_audit :: Lens.Lens' LogsSummary (Prelude.Maybe Prelude.Bool)
logsSummary_audit = Lens.lens (\LogsSummary' {audit} -> audit) (\s@LogsSummary' {} a -> s {audit = a} :: LogsSummary)

-- | The list of information about logs pending to be deployed for the
-- specified broker.
logsSummary_pending :: Lens.Lens' LogsSummary (Prelude.Maybe PendingLogs)
logsSummary_pending = Lens.lens (\LogsSummary' {pending} -> pending) (\s@LogsSummary' {} a -> s {pending = a} :: LogsSummary)

-- | The location of the CloudWatch Logs log group where general logs are
-- sent.
logsSummary_generalLogGroup :: Lens.Lens' LogsSummary Prelude.Text
logsSummary_generalLogGroup = Lens.lens (\LogsSummary' {generalLogGroup} -> generalLogGroup) (\s@LogsSummary' {} a -> s {generalLogGroup = a} :: LogsSummary)

-- | Enables general logging.
logsSummary_general :: Lens.Lens' LogsSummary Prelude.Bool
logsSummary_general = Lens.lens (\LogsSummary' {general} -> general) (\s@LogsSummary' {} a -> s {general = a} :: LogsSummary)

instance Data.FromJSON LogsSummary where
  parseJSON =
    Data.withObject
      "LogsSummary"
      ( \x ->
          LogsSummary'
            Prelude.<$> (x Data..:? "auditLogGroup")
            Prelude.<*> (x Data..:? "audit")
            Prelude.<*> (x Data..:? "pending")
            Prelude.<*> (x Data..: "generalLogGroup")
            Prelude.<*> (x Data..: "general")
      )

instance Prelude.Hashable LogsSummary where
  hashWithSalt _salt LogsSummary' {..} =
    _salt `Prelude.hashWithSalt` auditLogGroup
      `Prelude.hashWithSalt` audit
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` generalLogGroup
      `Prelude.hashWithSalt` general

instance Prelude.NFData LogsSummary where
  rnf LogsSummary' {..} =
    Prelude.rnf auditLogGroup
      `Prelude.seq` Prelude.rnf audit
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf generalLogGroup
      `Prelude.seq` Prelude.rnf general
