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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsPendingDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsPendingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about logs to be activated for the specified
-- broker.
--
-- /See:/ 'newAwsAmazonMqBrokerLogsPendingDetails' smart constructor.
data AwsAmazonMqBrokerLogsPendingDetails = AwsAmazonMqBrokerLogsPendingDetails'
  { -- | Activates audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | Activates general logging.
    general :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerLogsPendingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audit', 'awsAmazonMqBrokerLogsPendingDetails_audit' - Activates audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
--
-- 'general', 'awsAmazonMqBrokerLogsPendingDetails_general' - Activates general logging.
newAwsAmazonMqBrokerLogsPendingDetails ::
  AwsAmazonMqBrokerLogsPendingDetails
newAwsAmazonMqBrokerLogsPendingDetails =
  AwsAmazonMqBrokerLogsPendingDetails'
    { audit =
        Prelude.Nothing,
      general = Prelude.Nothing
    }

-- | Activates audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Doesn\'t apply to RabbitMQ brokers.
awsAmazonMqBrokerLogsPendingDetails_audit :: Lens.Lens' AwsAmazonMqBrokerLogsPendingDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLogsPendingDetails_audit = Lens.lens (\AwsAmazonMqBrokerLogsPendingDetails' {audit} -> audit) (\s@AwsAmazonMqBrokerLogsPendingDetails' {} a -> s {audit = a} :: AwsAmazonMqBrokerLogsPendingDetails)

-- | Activates general logging.
awsAmazonMqBrokerLogsPendingDetails_general :: Lens.Lens' AwsAmazonMqBrokerLogsPendingDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLogsPendingDetails_general = Lens.lens (\AwsAmazonMqBrokerLogsPendingDetails' {general} -> general) (\s@AwsAmazonMqBrokerLogsPendingDetails' {} a -> s {general = a} :: AwsAmazonMqBrokerLogsPendingDetails)

instance
  Data.FromJSON
    AwsAmazonMqBrokerLogsPendingDetails
  where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerLogsPendingDetails"
      ( \x ->
          AwsAmazonMqBrokerLogsPendingDetails'
            Prelude.<$> (x Data..:? "Audit")
            Prelude.<*> (x Data..:? "General")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerLogsPendingDetails
  where
  hashWithSalt
    _salt
    AwsAmazonMqBrokerLogsPendingDetails' {..} =
      _salt
        `Prelude.hashWithSalt` audit
        `Prelude.hashWithSalt` general

instance
  Prelude.NFData
    AwsAmazonMqBrokerLogsPendingDetails
  where
  rnf AwsAmazonMqBrokerLogsPendingDetails' {..} =
    Prelude.rnf audit `Prelude.seq` Prelude.rnf general

instance
  Data.ToJSON
    AwsAmazonMqBrokerLogsPendingDetails
  where
  toJSON AwsAmazonMqBrokerLogsPendingDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Audit" Data..=) Prelude.<$> audit,
            ("General" Data..=) Prelude.<$> general
          ]
      )
