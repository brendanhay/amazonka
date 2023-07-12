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
-- Module      : Amazonka.QuickSight.Types.LocalNavigationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LocalNavigationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The navigation configuration for @CustomActionNavigationOperation@.
--
-- /See:/ 'newLocalNavigationConfiguration' smart constructor.
data LocalNavigationConfiguration = LocalNavigationConfiguration'
  { -- | The sheet that is targeted for navigation in the same analysis.
    targetSheetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalNavigationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetSheetId', 'localNavigationConfiguration_targetSheetId' - The sheet that is targeted for navigation in the same analysis.
newLocalNavigationConfiguration ::
  -- | 'targetSheetId'
  Prelude.Text ->
  LocalNavigationConfiguration
newLocalNavigationConfiguration pTargetSheetId_ =
  LocalNavigationConfiguration'
    { targetSheetId =
        pTargetSheetId_
    }

-- | The sheet that is targeted for navigation in the same analysis.
localNavigationConfiguration_targetSheetId :: Lens.Lens' LocalNavigationConfiguration Prelude.Text
localNavigationConfiguration_targetSheetId = Lens.lens (\LocalNavigationConfiguration' {targetSheetId} -> targetSheetId) (\s@LocalNavigationConfiguration' {} a -> s {targetSheetId = a} :: LocalNavigationConfiguration)

instance Data.FromJSON LocalNavigationConfiguration where
  parseJSON =
    Data.withObject
      "LocalNavigationConfiguration"
      ( \x ->
          LocalNavigationConfiguration'
            Prelude.<$> (x Data..: "TargetSheetId")
      )

instance
  Prelude.Hashable
    LocalNavigationConfiguration
  where
  hashWithSalt _salt LocalNavigationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` targetSheetId

instance Prelude.NFData LocalNavigationConfiguration where
  rnf LocalNavigationConfiguration' {..} =
    Prelude.rnf targetSheetId

instance Data.ToJSON LocalNavigationConfiguration where
  toJSON LocalNavigationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TargetSheetId" Data..= targetSheetId)
          ]
      )
