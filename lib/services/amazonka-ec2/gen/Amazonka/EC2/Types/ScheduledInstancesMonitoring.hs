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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesMonitoring
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesMonitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes whether monitoring is enabled for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesMonitoring' smart constructor.
data ScheduledInstancesMonitoring = ScheduledInstancesMonitoring'
  { -- | Indicates whether monitoring is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'scheduledInstancesMonitoring_enabled' - Indicates whether monitoring is enabled.
newScheduledInstancesMonitoring ::
  ScheduledInstancesMonitoring
newScheduledInstancesMonitoring =
  ScheduledInstancesMonitoring'
    { enabled =
        Prelude.Nothing
    }

-- | Indicates whether monitoring is enabled.
scheduledInstancesMonitoring_enabled :: Lens.Lens' ScheduledInstancesMonitoring (Prelude.Maybe Prelude.Bool)
scheduledInstancesMonitoring_enabled = Lens.lens (\ScheduledInstancesMonitoring' {enabled} -> enabled) (\s@ScheduledInstancesMonitoring' {} a -> s {enabled = a} :: ScheduledInstancesMonitoring)

instance
  Prelude.Hashable
    ScheduledInstancesMonitoring
  where
  hashWithSalt _salt ScheduledInstancesMonitoring' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData ScheduledInstancesMonitoring where
  rnf ScheduledInstancesMonitoring' {..} =
    Prelude.rnf enabled

instance Data.ToQuery ScheduledInstancesMonitoring where
  toQuery ScheduledInstancesMonitoring' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
