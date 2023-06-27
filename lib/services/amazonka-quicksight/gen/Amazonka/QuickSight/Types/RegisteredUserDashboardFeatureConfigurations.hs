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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserDashboardFeatureConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserDashboardFeatureConfigurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BookmarksConfigurations
import Amazonka.QuickSight.Types.StatePersistenceConfigurations

-- | The feature configuration for an embedded dashboard.
--
-- /See:/ 'newRegisteredUserDashboardFeatureConfigurations' smart constructor.
data RegisteredUserDashboardFeatureConfigurations = RegisteredUserDashboardFeatureConfigurations'
  { -- | The bookmarks configuration for an embedded dashboard in Amazon
    -- QuickSight.
    bookmarks :: Prelude.Maybe BookmarksConfigurations,
    -- | The state persistence settings of an embedded dashboard.
    statePersistence :: Prelude.Maybe StatePersistenceConfigurations
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserDashboardFeatureConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bookmarks', 'registeredUserDashboardFeatureConfigurations_bookmarks' - The bookmarks configuration for an embedded dashboard in Amazon
-- QuickSight.
--
-- 'statePersistence', 'registeredUserDashboardFeatureConfigurations_statePersistence' - The state persistence settings of an embedded dashboard.
newRegisteredUserDashboardFeatureConfigurations ::
  RegisteredUserDashboardFeatureConfigurations
newRegisteredUserDashboardFeatureConfigurations =
  RegisteredUserDashboardFeatureConfigurations'
    { bookmarks =
        Prelude.Nothing,
      statePersistence =
        Prelude.Nothing
    }

-- | The bookmarks configuration for an embedded dashboard in Amazon
-- QuickSight.
registeredUserDashboardFeatureConfigurations_bookmarks :: Lens.Lens' RegisteredUserDashboardFeatureConfigurations (Prelude.Maybe BookmarksConfigurations)
registeredUserDashboardFeatureConfigurations_bookmarks = Lens.lens (\RegisteredUserDashboardFeatureConfigurations' {bookmarks} -> bookmarks) (\s@RegisteredUserDashboardFeatureConfigurations' {} a -> s {bookmarks = a} :: RegisteredUserDashboardFeatureConfigurations)

-- | The state persistence settings of an embedded dashboard.
registeredUserDashboardFeatureConfigurations_statePersistence :: Lens.Lens' RegisteredUserDashboardFeatureConfigurations (Prelude.Maybe StatePersistenceConfigurations)
registeredUserDashboardFeatureConfigurations_statePersistence = Lens.lens (\RegisteredUserDashboardFeatureConfigurations' {statePersistence} -> statePersistence) (\s@RegisteredUserDashboardFeatureConfigurations' {} a -> s {statePersistence = a} :: RegisteredUserDashboardFeatureConfigurations)

instance
  Prelude.Hashable
    RegisteredUserDashboardFeatureConfigurations
  where
  hashWithSalt
    _salt
    RegisteredUserDashboardFeatureConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` bookmarks
        `Prelude.hashWithSalt` statePersistence

instance
  Prelude.NFData
    RegisteredUserDashboardFeatureConfigurations
  where
  rnf RegisteredUserDashboardFeatureConfigurations' {..} =
    Prelude.rnf bookmarks
      `Prelude.seq` Prelude.rnf statePersistence

instance
  Data.ToJSON
    RegisteredUserDashboardFeatureConfigurations
  where
  toJSON
    RegisteredUserDashboardFeatureConfigurations' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Bookmarks" Data..=) Prelude.<$> bookmarks,
              ("StatePersistence" Data..=)
                Prelude.<$> statePersistence
            ]
        )
