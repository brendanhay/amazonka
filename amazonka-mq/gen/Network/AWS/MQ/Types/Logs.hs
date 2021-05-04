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
-- Module      : Network.AWS.MQ.Types.Logs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Logs where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The list of information about logs to be enabled for the specified
-- broker.
--
-- /See:/ 'newLogs' smart constructor.
data Logs = Logs'
  { -- | Enables general logging.
    general :: Prelude.Maybe Prelude.Bool,
    -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
    audit :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Logs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'general', 'logs_general' - Enables general logging.
--
-- 'audit', 'logs_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
newLogs ::
  Logs
newLogs =
  Logs'
    { general = Prelude.Nothing,
      audit = Prelude.Nothing
    }

-- | Enables general logging.
logs_general :: Lens.Lens' Logs (Prelude.Maybe Prelude.Bool)
logs_general = Lens.lens (\Logs' {general} -> general) (\s@Logs' {} a -> s {general = a} :: Logs)

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged. Does not apply to RabbitMQ brokers.
logs_audit :: Lens.Lens' Logs (Prelude.Maybe Prelude.Bool)
logs_audit = Lens.lens (\Logs' {audit} -> audit) (\s@Logs' {} a -> s {audit = a} :: Logs)

instance Prelude.FromJSON Logs where
  parseJSON =
    Prelude.withObject
      "Logs"
      ( \x ->
          Logs'
            Prelude.<$> (x Prelude..:? "general")
            Prelude.<*> (x Prelude..:? "audit")
      )

instance Prelude.Hashable Logs

instance Prelude.NFData Logs

instance Prelude.ToJSON Logs where
  toJSON Logs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("general" Prelude..=) Prelude.<$> general,
            ("audit" Prelude..=) Prelude.<$> audit
          ]
      )
