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
-- Module      : Amazonka.RDS.Types.PendingCloudwatchLogsExports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.PendingCloudwatchLogsExports where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of the log types whose configuration is still pending. In other
-- words, these log types are in the process of being activated or
-- deactivated.
--
-- /See:/ 'newPendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { -- | Log types that are in the process of being enabled. After they are
    -- enabled, these log types are exported to CloudWatch Logs.
    logTypesToDisable :: Prelude.Maybe [Prelude.Text],
    -- | Log types that are in the process of being deactivated. After they are
    -- deactivated, these log types aren\'t exported to CloudWatch Logs.
    logTypesToEnable :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingCloudwatchLogsExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTypesToDisable', 'pendingCloudwatchLogsExports_logTypesToDisable' - Log types that are in the process of being enabled. After they are
-- enabled, these log types are exported to CloudWatch Logs.
--
-- 'logTypesToEnable', 'pendingCloudwatchLogsExports_logTypesToEnable' - Log types that are in the process of being deactivated. After they are
-- deactivated, these log types aren\'t exported to CloudWatch Logs.
newPendingCloudwatchLogsExports ::
  PendingCloudwatchLogsExports
newPendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    { logTypesToDisable =
        Prelude.Nothing,
      logTypesToEnable = Prelude.Nothing
    }

-- | Log types that are in the process of being enabled. After they are
-- enabled, these log types are exported to CloudWatch Logs.
pendingCloudwatchLogsExports_logTypesToDisable :: Lens.Lens' PendingCloudwatchLogsExports (Prelude.Maybe [Prelude.Text])
pendingCloudwatchLogsExports_logTypesToDisable = Lens.lens (\PendingCloudwatchLogsExports' {logTypesToDisable} -> logTypesToDisable) (\s@PendingCloudwatchLogsExports' {} a -> s {logTypesToDisable = a} :: PendingCloudwatchLogsExports) Prelude.. Lens.mapping Lens.coerced

-- | Log types that are in the process of being deactivated. After they are
-- deactivated, these log types aren\'t exported to CloudWatch Logs.
pendingCloudwatchLogsExports_logTypesToEnable :: Lens.Lens' PendingCloudwatchLogsExports (Prelude.Maybe [Prelude.Text])
pendingCloudwatchLogsExports_logTypesToEnable = Lens.lens (\PendingCloudwatchLogsExports' {logTypesToEnable} -> logTypesToEnable) (\s@PendingCloudwatchLogsExports' {} a -> s {logTypesToEnable = a} :: PendingCloudwatchLogsExports) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PendingCloudwatchLogsExports where
  parseXML x =
    PendingCloudwatchLogsExports'
      Prelude.<$> ( x Data..@? "LogTypesToDisable"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "LogTypesToEnable"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    PendingCloudwatchLogsExports
  where
  hashWithSalt _salt PendingCloudwatchLogsExports' {..} =
    _salt `Prelude.hashWithSalt` logTypesToDisable
      `Prelude.hashWithSalt` logTypesToEnable

instance Prelude.NFData PendingCloudwatchLogsExports where
  rnf PendingCloudwatchLogsExports' {..} =
    Prelude.rnf logTypesToDisable
      `Prelude.seq` Prelude.rnf logTypesToEnable
