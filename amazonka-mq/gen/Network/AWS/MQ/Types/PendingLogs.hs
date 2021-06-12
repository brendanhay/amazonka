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
-- Module      : Network.AWS.MQ.Types.PendingLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.PendingLogs where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The list of information about logs to be enabled for the specified
-- broker.
--
-- /See:/ 'newPendingLogs' smart constructor.
data PendingLogs = PendingLogs'
  { -- | Enables general logging.
    general :: Core.Maybe Core.Bool,
    -- | Enables audit logging. Every user management action made using JMX or
    -- the ActiveMQ Web Console is logged.
    audit :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PendingLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'general', 'pendingLogs_general' - Enables general logging.
--
-- 'audit', 'pendingLogs_audit' - Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
newPendingLogs ::
  PendingLogs
newPendingLogs =
  PendingLogs'
    { general = Core.Nothing,
      audit = Core.Nothing
    }

-- | Enables general logging.
pendingLogs_general :: Lens.Lens' PendingLogs (Core.Maybe Core.Bool)
pendingLogs_general = Lens.lens (\PendingLogs' {general} -> general) (\s@PendingLogs' {} a -> s {general = a} :: PendingLogs)

-- | Enables audit logging. Every user management action made using JMX or
-- the ActiveMQ Web Console is logged.
pendingLogs_audit :: Lens.Lens' PendingLogs (Core.Maybe Core.Bool)
pendingLogs_audit = Lens.lens (\PendingLogs' {audit} -> audit) (\s@PendingLogs' {} a -> s {audit = a} :: PendingLogs)

instance Core.FromJSON PendingLogs where
  parseJSON =
    Core.withObject
      "PendingLogs"
      ( \x ->
          PendingLogs'
            Core.<$> (x Core..:? "general") Core.<*> (x Core..:? "audit")
      )

instance Core.Hashable PendingLogs

instance Core.NFData PendingLogs
