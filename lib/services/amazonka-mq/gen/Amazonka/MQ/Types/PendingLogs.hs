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
-- Module      : Amazonka.MQ.Types.PendingLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.PendingLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of information about logs to be enabled for the specified
-- broker.
--
-- /See:/ 'newPendingLogs' smart constructor.
data PendingLogs = PendingLogs'
  { -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged.
    audit :: Prelude.Maybe Prelude.Bool,
    -- | Enables general logging.
    general :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audit', 'pendingLogs_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
--
-- 'general', 'pendingLogs_general' - Enables general logging.
newPendingLogs ::
  PendingLogs
newPendingLogs =
  PendingLogs'
    { audit = Prelude.Nothing,
      general = Prelude.Nothing
    }

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
pendingLogs_audit :: Lens.Lens' PendingLogs (Prelude.Maybe Prelude.Bool)
pendingLogs_audit = Lens.lens (\PendingLogs' {audit} -> audit) (\s@PendingLogs' {} a -> s {audit = a} :: PendingLogs)

-- | Enables general logging.
pendingLogs_general :: Lens.Lens' PendingLogs (Prelude.Maybe Prelude.Bool)
pendingLogs_general = Lens.lens (\PendingLogs' {general} -> general) (\s@PendingLogs' {} a -> s {general = a} :: PendingLogs)

instance Data.FromJSON PendingLogs where
  parseJSON =
    Data.withObject
      "PendingLogs"
      ( \x ->
          PendingLogs'
            Prelude.<$> (x Data..:? "audit")
            Prelude.<*> (x Data..:? "general")
      )

instance Prelude.Hashable PendingLogs where
  hashWithSalt _salt PendingLogs' {..} =
    _salt `Prelude.hashWithSalt` audit
      `Prelude.hashWithSalt` general

instance Prelude.NFData PendingLogs where
  rnf PendingLogs' {..} =
    Prelude.rnf audit `Prelude.seq` Prelude.rnf general
