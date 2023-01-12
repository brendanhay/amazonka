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
-- Module      : Amazonka.EC2.Types.Monitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Monitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.MonitoringState
import qualified Amazonka.Prelude as Prelude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'newMonitoring' smart constructor.
data Monitoring = Monitoring'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic
    -- monitoring is enabled.
    state :: Prelude.Maybe MonitoringState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Monitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'monitoring_state' - Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
newMonitoring ::
  Monitoring
newMonitoring = Monitoring' {state = Prelude.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
monitoring_state :: Lens.Lens' Monitoring (Prelude.Maybe MonitoringState)
monitoring_state = Lens.lens (\Monitoring' {state} -> state) (\s@Monitoring' {} a -> s {state = a} :: Monitoring)

instance Data.FromXML Monitoring where
  parseXML x =
    Monitoring' Prelude.<$> (x Data..@? "state")

instance Prelude.Hashable Monitoring where
  hashWithSalt _salt Monitoring' {..} =
    _salt `Prelude.hashWithSalt` state

instance Prelude.NFData Monitoring where
  rnf Monitoring' {..} = Prelude.rnf state
