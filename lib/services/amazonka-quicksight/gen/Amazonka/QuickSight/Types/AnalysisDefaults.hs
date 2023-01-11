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
-- Module      : Amazonka.QuickSight.Types.AnalysisDefaults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisDefaults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DefaultNewSheetConfiguration

-- | The configuration for default analysis settings.
--
-- /See:/ 'newAnalysisDefaults' smart constructor.
data AnalysisDefaults = AnalysisDefaults'
  { -- | The configuration for default new sheet settings.
    defaultNewSheetConfiguration :: DefaultNewSheetConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultNewSheetConfiguration', 'analysisDefaults_defaultNewSheetConfiguration' - The configuration for default new sheet settings.
newAnalysisDefaults ::
  -- | 'defaultNewSheetConfiguration'
  DefaultNewSheetConfiguration ->
  AnalysisDefaults
newAnalysisDefaults pDefaultNewSheetConfiguration_ =
  AnalysisDefaults'
    { defaultNewSheetConfiguration =
        pDefaultNewSheetConfiguration_
    }

-- | The configuration for default new sheet settings.
analysisDefaults_defaultNewSheetConfiguration :: Lens.Lens' AnalysisDefaults DefaultNewSheetConfiguration
analysisDefaults_defaultNewSheetConfiguration = Lens.lens (\AnalysisDefaults' {defaultNewSheetConfiguration} -> defaultNewSheetConfiguration) (\s@AnalysisDefaults' {} a -> s {defaultNewSheetConfiguration = a} :: AnalysisDefaults)

instance Data.FromJSON AnalysisDefaults where
  parseJSON =
    Data.withObject
      "AnalysisDefaults"
      ( \x ->
          AnalysisDefaults'
            Prelude.<$> (x Data..: "DefaultNewSheetConfiguration")
      )

instance Prelude.Hashable AnalysisDefaults where
  hashWithSalt _salt AnalysisDefaults' {..} =
    _salt
      `Prelude.hashWithSalt` defaultNewSheetConfiguration

instance Prelude.NFData AnalysisDefaults where
  rnf AnalysisDefaults' {..} =
    Prelude.rnf defaultNewSheetConfiguration

instance Data.ToJSON AnalysisDefaults where
  toJSON AnalysisDefaults' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DefaultNewSheetConfiguration"
                  Data..= defaultNewSheetConfiguration
              )
          ]
      )
