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
-- Module      : Amazonka.EC2.Types.RunInstancesMonitoringEnabled
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RunInstancesMonitoringEnabled where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'newRunInstancesMonitoringEnabled' smart constructor.
data RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic
    -- monitoring is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunInstancesMonitoringEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'runInstancesMonitoringEnabled_enabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
newRunInstancesMonitoringEnabled ::
  -- | 'enabled'
  Prelude.Bool ->
  RunInstancesMonitoringEnabled
newRunInstancesMonitoringEnabled pEnabled_ =
  RunInstancesMonitoringEnabled' {enabled = pEnabled_}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
runInstancesMonitoringEnabled_enabled :: Lens.Lens' RunInstancesMonitoringEnabled Prelude.Bool
runInstancesMonitoringEnabled_enabled = Lens.lens (\RunInstancesMonitoringEnabled' {enabled} -> enabled) (\s@RunInstancesMonitoringEnabled' {} a -> s {enabled = a} :: RunInstancesMonitoringEnabled)

instance Data.FromXML RunInstancesMonitoringEnabled where
  parseXML x =
    RunInstancesMonitoringEnabled'
      Prelude.<$> (x Data..@ "enabled")

instance
  Prelude.Hashable
    RunInstancesMonitoringEnabled
  where
  hashWithSalt _salt RunInstancesMonitoringEnabled' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData RunInstancesMonitoringEnabled where
  rnf RunInstancesMonitoringEnabled' {..} =
    Prelude.rnf enabled

instance Data.ToQuery RunInstancesMonitoringEnabled where
  toQuery RunInstancesMonitoringEnabled' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
