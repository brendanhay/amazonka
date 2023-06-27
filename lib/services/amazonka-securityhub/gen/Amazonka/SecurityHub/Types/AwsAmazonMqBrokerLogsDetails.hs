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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsPendingDetails

-- | Provides information about logs to be activated for the specified
-- broker.
--
-- /See:/ 'newAwsAmazonMqBrokerLogsDetails' smart constructor.
data AwsAmazonMqBrokerLogsDetails = AwsAmazonMqBrokerLogsDetails'
  { -- | Activates audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | The location of the CloudWatch Logs log group where audit logs are sent.
    auditLogGroup :: Prelude.Maybe Prelude.Text,
    -- | Activates general logging.
    general :: Prelude.Maybe Prelude.Bool,
    -- | The location of the CloudWatch Logs log group where general logs are
    -- sent.
    generalLogGroup :: Prelude.Maybe Prelude.Text,
    -- | The list of information about logs that are to be turned on for the
    -- specified broker.
    pending :: Prelude.Maybe AwsAmazonMqBrokerLogsPendingDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audit', 'awsAmazonMqBrokerLogsDetails_audit' - Activates audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
--
-- 'auditLogGroup', 'awsAmazonMqBrokerLogsDetails_auditLogGroup' - The location of the CloudWatch Logs log group where audit logs are sent.
--
-- 'general', 'awsAmazonMqBrokerLogsDetails_general' - Activates general logging.
--
-- 'generalLogGroup', 'awsAmazonMqBrokerLogsDetails_generalLogGroup' - The location of the CloudWatch Logs log group where general logs are
-- sent.
--
-- 'pending', 'awsAmazonMqBrokerLogsDetails_pending' - The list of information about logs that are to be turned on for the
-- specified broker.
newAwsAmazonMqBrokerLogsDetails ::
  AwsAmazonMqBrokerLogsDetails
newAwsAmazonMqBrokerLogsDetails =
  AwsAmazonMqBrokerLogsDetails'
    { audit =
        Prelude.Nothing,
      auditLogGroup = Prelude.Nothing,
      general = Prelude.Nothing,
      generalLogGroup = Prelude.Nothing,
      pending = Prelude.Nothing
    }

-- | Activates audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
awsAmazonMqBrokerLogsDetails_audit :: Lens.Lens' AwsAmazonMqBrokerLogsDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLogsDetails_audit = Lens.lens (\AwsAmazonMqBrokerLogsDetails' {audit} -> audit) (\s@AwsAmazonMqBrokerLogsDetails' {} a -> s {audit = a} :: AwsAmazonMqBrokerLogsDetails)

-- | The location of the CloudWatch Logs log group where audit logs are sent.
awsAmazonMqBrokerLogsDetails_auditLogGroup :: Lens.Lens' AwsAmazonMqBrokerLogsDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLogsDetails_auditLogGroup = Lens.lens (\AwsAmazonMqBrokerLogsDetails' {auditLogGroup} -> auditLogGroup) (\s@AwsAmazonMqBrokerLogsDetails' {} a -> s {auditLogGroup = a} :: AwsAmazonMqBrokerLogsDetails)

-- | Activates general logging.
awsAmazonMqBrokerLogsDetails_general :: Lens.Lens' AwsAmazonMqBrokerLogsDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLogsDetails_general = Lens.lens (\AwsAmazonMqBrokerLogsDetails' {general} -> general) (\s@AwsAmazonMqBrokerLogsDetails' {} a -> s {general = a} :: AwsAmazonMqBrokerLogsDetails)

-- | The location of the CloudWatch Logs log group where general logs are
-- sent.
awsAmazonMqBrokerLogsDetails_generalLogGroup :: Lens.Lens' AwsAmazonMqBrokerLogsDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLogsDetails_generalLogGroup = Lens.lens (\AwsAmazonMqBrokerLogsDetails' {generalLogGroup} -> generalLogGroup) (\s@AwsAmazonMqBrokerLogsDetails' {} a -> s {generalLogGroup = a} :: AwsAmazonMqBrokerLogsDetails)

-- | The list of information about logs that are to be turned on for the
-- specified broker.
awsAmazonMqBrokerLogsDetails_pending :: Lens.Lens' AwsAmazonMqBrokerLogsDetails (Prelude.Maybe AwsAmazonMqBrokerLogsPendingDetails)
awsAmazonMqBrokerLogsDetails_pending = Lens.lens (\AwsAmazonMqBrokerLogsDetails' {pending} -> pending) (\s@AwsAmazonMqBrokerLogsDetails' {} a -> s {pending = a} :: AwsAmazonMqBrokerLogsDetails)

instance Data.FromJSON AwsAmazonMqBrokerLogsDetails where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerLogsDetails"
      ( \x ->
          AwsAmazonMqBrokerLogsDetails'
            Prelude.<$> (x Data..:? "Audit")
            Prelude.<*> (x Data..:? "AuditLogGroup")
            Prelude.<*> (x Data..:? "General")
            Prelude.<*> (x Data..:? "GeneralLogGroup")
            Prelude.<*> (x Data..:? "Pending")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerLogsDetails
  where
  hashWithSalt _salt AwsAmazonMqBrokerLogsDetails' {..} =
    _salt
      `Prelude.hashWithSalt` audit
      `Prelude.hashWithSalt` auditLogGroup
      `Prelude.hashWithSalt` general
      `Prelude.hashWithSalt` generalLogGroup
      `Prelude.hashWithSalt` pending

instance Prelude.NFData AwsAmazonMqBrokerLogsDetails where
  rnf AwsAmazonMqBrokerLogsDetails' {..} =
    Prelude.rnf audit
      `Prelude.seq` Prelude.rnf auditLogGroup
      `Prelude.seq` Prelude.rnf general
      `Prelude.seq` Prelude.rnf generalLogGroup
      `Prelude.seq` Prelude.rnf pending

instance Data.ToJSON AwsAmazonMqBrokerLogsDetails where
  toJSON AwsAmazonMqBrokerLogsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Audit" Data..=) Prelude.<$> audit,
            ("AuditLogGroup" Data..=) Prelude.<$> auditLogGroup,
            ("General" Data..=) Prelude.<$> general,
            ("GeneralLogGroup" Data..=)
              Prelude.<$> generalLogGroup,
            ("Pending" Data..=) Prelude.<$> pending
          ]
      )
