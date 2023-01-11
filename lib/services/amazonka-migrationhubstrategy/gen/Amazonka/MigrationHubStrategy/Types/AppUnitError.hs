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
-- Module      : Amazonka.MigrationHubStrategy.Types.AppUnitError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AppUnitError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AppUnitErrorCategory
import qualified Amazonka.Prelude as Prelude

-- | Error in the analysis of the application unit.
--
-- /See:/ 'newAppUnitError' smart constructor.
data AppUnitError = AppUnitError'
  { -- | The category of the error.
    appUnitErrorCategory :: Prelude.Maybe AppUnitErrorCategory
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppUnitError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appUnitErrorCategory', 'appUnitError_appUnitErrorCategory' - The category of the error.
newAppUnitError ::
  AppUnitError
newAppUnitError =
  AppUnitError'
    { appUnitErrorCategory =
        Prelude.Nothing
    }

-- | The category of the error.
appUnitError_appUnitErrorCategory :: Lens.Lens' AppUnitError (Prelude.Maybe AppUnitErrorCategory)
appUnitError_appUnitErrorCategory = Lens.lens (\AppUnitError' {appUnitErrorCategory} -> appUnitErrorCategory) (\s@AppUnitError' {} a -> s {appUnitErrorCategory = a} :: AppUnitError)

instance Data.FromJSON AppUnitError where
  parseJSON =
    Data.withObject
      "AppUnitError"
      ( \x ->
          AppUnitError'
            Prelude.<$> (x Data..:? "appUnitErrorCategory")
      )

instance Prelude.Hashable AppUnitError where
  hashWithSalt _salt AppUnitError' {..} =
    _salt `Prelude.hashWithSalt` appUnitErrorCategory

instance Prelude.NFData AppUnitError where
  rnf AppUnitError' {..} =
    Prelude.rnf appUnitErrorCategory
