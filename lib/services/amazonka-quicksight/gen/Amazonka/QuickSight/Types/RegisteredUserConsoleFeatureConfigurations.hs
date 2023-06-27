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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserConsoleFeatureConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserConsoleFeatureConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.StatePersistenceConfigurations

-- | The feature configurations of an embedded Amazon QuickSight console.
--
-- /See:/ 'newRegisteredUserConsoleFeatureConfigurations' smart constructor.
data RegisteredUserConsoleFeatureConfigurations = RegisteredUserConsoleFeatureConfigurations'
  { -- | The state persistence configurations of an embedded Amazon QuickSight
    -- console.
    statePersistence :: Prelude.Maybe StatePersistenceConfigurations
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserConsoleFeatureConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statePersistence', 'registeredUserConsoleFeatureConfigurations_statePersistence' - The state persistence configurations of an embedded Amazon QuickSight
-- console.
newRegisteredUserConsoleFeatureConfigurations ::
  RegisteredUserConsoleFeatureConfigurations
newRegisteredUserConsoleFeatureConfigurations =
  RegisteredUserConsoleFeatureConfigurations'
    { statePersistence =
        Prelude.Nothing
    }

-- | The state persistence configurations of an embedded Amazon QuickSight
-- console.
registeredUserConsoleFeatureConfigurations_statePersistence :: Lens.Lens' RegisteredUserConsoleFeatureConfigurations (Prelude.Maybe StatePersistenceConfigurations)
registeredUserConsoleFeatureConfigurations_statePersistence = Lens.lens (\RegisteredUserConsoleFeatureConfigurations' {statePersistence} -> statePersistence) (\s@RegisteredUserConsoleFeatureConfigurations' {} a -> s {statePersistence = a} :: RegisteredUserConsoleFeatureConfigurations)

instance
  Prelude.Hashable
    RegisteredUserConsoleFeatureConfigurations
  where
  hashWithSalt
    _salt
    RegisteredUserConsoleFeatureConfigurations' {..} =
      _salt `Prelude.hashWithSalt` statePersistence

instance
  Prelude.NFData
    RegisteredUserConsoleFeatureConfigurations
  where
  rnf RegisteredUserConsoleFeatureConfigurations' {..} =
    Prelude.rnf statePersistence

instance
  Data.ToJSON
    RegisteredUserConsoleFeatureConfigurations
  where
  toJSON
    RegisteredUserConsoleFeatureConfigurations' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("StatePersistence" Data..=)
                Prelude.<$> statePersistence
            ]
        )
