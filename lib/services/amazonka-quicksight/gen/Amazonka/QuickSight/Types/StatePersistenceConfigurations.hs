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
-- Module      : Amazonka.QuickSight.Types.StatePersistenceConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StatePersistenceConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state perssitence configuration of an embedded dashboard.
--
-- /See:/ 'newStatePersistenceConfigurations' smart constructor.
data StatePersistenceConfigurations = StatePersistenceConfigurations'
  { -- | Determines if a Amazon QuickSight dashboard\'s state persistence
    -- settings are turned on or off.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatePersistenceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'statePersistenceConfigurations_enabled' - Determines if a Amazon QuickSight dashboard\'s state persistence
-- settings are turned on or off.
newStatePersistenceConfigurations ::
  -- | 'enabled'
  Prelude.Bool ->
  StatePersistenceConfigurations
newStatePersistenceConfigurations pEnabled_ =
  StatePersistenceConfigurations'
    { enabled =
        pEnabled_
    }

-- | Determines if a Amazon QuickSight dashboard\'s state persistence
-- settings are turned on or off.
statePersistenceConfigurations_enabled :: Lens.Lens' StatePersistenceConfigurations Prelude.Bool
statePersistenceConfigurations_enabled = Lens.lens (\StatePersistenceConfigurations' {enabled} -> enabled) (\s@StatePersistenceConfigurations' {} a -> s {enabled = a} :: StatePersistenceConfigurations)

instance
  Prelude.Hashable
    StatePersistenceConfigurations
  where
  hashWithSalt
    _salt
    StatePersistenceConfigurations' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    StatePersistenceConfigurations
  where
  rnf StatePersistenceConfigurations' {..} =
    Prelude.rnf enabled

instance Data.ToJSON StatePersistenceConfigurations where
  toJSON StatePersistenceConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Enabled" Data..= enabled)]
      )
