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
-- Module      : Network.AWS.EKS.Types.LogSetup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.LogSetup where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.LogType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the enabled or disabled Kubernetes control plane
-- logs for your cluster.
--
-- /See:/ 'newLogSetup' smart constructor.
data LogSetup = LogSetup'
  { -- | If a log type is enabled, that log type exports its control plane logs
    -- to CloudWatch Logs. If a log type isn\'t enabled, that log type doesn\'t
    -- export its control plane logs. Each individual log type can be enabled
    -- or disabled independently.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The available cluster control plane log types.
    types :: Prelude.Maybe [LogType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSetup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'logSetup_enabled' - If a log type is enabled, that log type exports its control plane logs
-- to CloudWatch Logs. If a log type isn\'t enabled, that log type doesn\'t
-- export its control plane logs. Each individual log type can be enabled
-- or disabled independently.
--
-- 'types', 'logSetup_types' - The available cluster control plane log types.
newLogSetup ::
  LogSetup
newLogSetup =
  LogSetup'
    { enabled = Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | If a log type is enabled, that log type exports its control plane logs
-- to CloudWatch Logs. If a log type isn\'t enabled, that log type doesn\'t
-- export its control plane logs. Each individual log type can be enabled
-- or disabled independently.
logSetup_enabled :: Lens.Lens' LogSetup (Prelude.Maybe Prelude.Bool)
logSetup_enabled = Lens.lens (\LogSetup' {enabled} -> enabled) (\s@LogSetup' {} a -> s {enabled = a} :: LogSetup)

-- | The available cluster control plane log types.
logSetup_types :: Lens.Lens' LogSetup (Prelude.Maybe [LogType])
logSetup_types = Lens.lens (\LogSetup' {types} -> types) (\s@LogSetup' {} a -> s {types = a} :: LogSetup) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON LogSetup where
  parseJSON =
    Core.withObject
      "LogSetup"
      ( \x ->
          LogSetup'
            Prelude.<$> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "types" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable LogSetup

instance Prelude.NFData LogSetup

instance Core.ToJSON LogSetup where
  toJSON LogSetup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enabled" Core..=) Prelude.<$> enabled,
            ("types" Core..=) Prelude.<$> types
          ]
      )
