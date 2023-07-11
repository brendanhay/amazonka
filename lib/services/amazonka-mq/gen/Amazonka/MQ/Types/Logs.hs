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
-- Module      : Amazonka.MQ.Types.Logs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.Logs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of information about logs to be enabled for the specified
-- broker.
--
-- /See:/ 'newLogs' smart constructor.
data Logs = Logs'
  { -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | Enables general logging.
    general :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Logs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audit', 'logs_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
--
-- 'general', 'logs_general' - Enables general logging.
newLogs ::
  Logs
newLogs =
  Logs'
    { audit = Prelude.Nothing,
      general = Prelude.Nothing
    }

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
logs_audit :: Lens.Lens' Logs (Prelude.Maybe Prelude.Bool)
logs_audit = Lens.lens (\Logs' {audit} -> audit) (\s@Logs' {} a -> s {audit = a} :: Logs)

-- | Enables general logging.
logs_general :: Lens.Lens' Logs (Prelude.Maybe Prelude.Bool)
logs_general = Lens.lens (\Logs' {general} -> general) (\s@Logs' {} a -> s {general = a} :: Logs)

instance Data.FromJSON Logs where
  parseJSON =
    Data.withObject
      "Logs"
      ( \x ->
          Logs'
            Prelude.<$> (x Data..:? "audit")
            Prelude.<*> (x Data..:? "general")
      )

instance Prelude.Hashable Logs where
  hashWithSalt _salt Logs' {..} =
    _salt
      `Prelude.hashWithSalt` audit
      `Prelude.hashWithSalt` general

instance Prelude.NFData Logs where
  rnf Logs' {..} =
    Prelude.rnf audit `Prelude.seq` Prelude.rnf general

instance Data.ToJSON Logs where
  toJSON Logs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audit" Data..=) Prelude.<$> audit,
            ("general" Data..=) Prelude.<$> general
          ]
      )
