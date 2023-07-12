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
-- Module      : Amazonka.EKS.Types.LogSetup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.LogSetup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.LogType
import qualified Amazonka.Prelude as Prelude

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
logSetup_types = Lens.lens (\LogSetup' {types} -> types) (\s@LogSetup' {} a -> s {types = a} :: LogSetup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LogSetup where
  parseJSON =
    Data.withObject
      "LogSetup"
      ( \x ->
          LogSetup'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "types" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LogSetup where
  hashWithSalt _salt LogSetup' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` types

instance Prelude.NFData LogSetup where
  rnf LogSetup' {..} =
    Prelude.rnf enabled `Prelude.seq` Prelude.rnf types

instance Data.ToJSON LogSetup where
  toJSON LogSetup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("types" Data..=) Prelude.<$> types
          ]
      )
