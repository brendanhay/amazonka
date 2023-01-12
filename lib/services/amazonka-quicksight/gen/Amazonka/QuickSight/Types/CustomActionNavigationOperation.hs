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
-- Module      : Amazonka.QuickSight.Types.CustomActionNavigationOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomActionNavigationOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LocalNavigationConfiguration

-- | The navigation operation that navigates between different sheets in the
-- same analysis.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newCustomActionNavigationOperation' smart constructor.
data CustomActionNavigationOperation = CustomActionNavigationOperation'
  { -- | The configuration that chooses the navigation target.
    localNavigationConfiguration :: Prelude.Maybe LocalNavigationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomActionNavigationOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localNavigationConfiguration', 'customActionNavigationOperation_localNavigationConfiguration' - The configuration that chooses the navigation target.
newCustomActionNavigationOperation ::
  CustomActionNavigationOperation
newCustomActionNavigationOperation =
  CustomActionNavigationOperation'
    { localNavigationConfiguration =
        Prelude.Nothing
    }

-- | The configuration that chooses the navigation target.
customActionNavigationOperation_localNavigationConfiguration :: Lens.Lens' CustomActionNavigationOperation (Prelude.Maybe LocalNavigationConfiguration)
customActionNavigationOperation_localNavigationConfiguration = Lens.lens (\CustomActionNavigationOperation' {localNavigationConfiguration} -> localNavigationConfiguration) (\s@CustomActionNavigationOperation' {} a -> s {localNavigationConfiguration = a} :: CustomActionNavigationOperation)

instance
  Data.FromJSON
    CustomActionNavigationOperation
  where
  parseJSON =
    Data.withObject
      "CustomActionNavigationOperation"
      ( \x ->
          CustomActionNavigationOperation'
            Prelude.<$> (x Data..:? "LocalNavigationConfiguration")
      )

instance
  Prelude.Hashable
    CustomActionNavigationOperation
  where
  hashWithSalt
    _salt
    CustomActionNavigationOperation' {..} =
      _salt
        `Prelude.hashWithSalt` localNavigationConfiguration

instance
  Prelude.NFData
    CustomActionNavigationOperation
  where
  rnf CustomActionNavigationOperation' {..} =
    Prelude.rnf localNavigationConfiguration

instance Data.ToJSON CustomActionNavigationOperation where
  toJSON CustomActionNavigationOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LocalNavigationConfiguration" Data..=)
              Prelude.<$> localNavigationConfiguration
          ]
      )
