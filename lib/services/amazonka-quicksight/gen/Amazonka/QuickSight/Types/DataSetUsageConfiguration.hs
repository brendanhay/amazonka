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
-- Module      : Amazonka.QuickSight.Types.DataSetUsageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetUsageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The usage configuration to apply to child datasets that reference this
-- dataset as a source.
--
-- /See:/ 'newDataSetUsageConfiguration' smart constructor.
data DataSetUsageConfiguration = DataSetUsageConfiguration'
  { -- | An option that controls whether a child dataset of a direct query can
    -- use this dataset as a source.
    disableUseAsDirectQuerySource :: Prelude.Maybe Prelude.Bool,
    -- | An option that controls whether a child dataset that\'s stored in
    -- QuickSight can use this dataset as a source.
    disableUseAsImportedSource :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetUsageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableUseAsDirectQuerySource', 'dataSetUsageConfiguration_disableUseAsDirectQuerySource' - An option that controls whether a child dataset of a direct query can
-- use this dataset as a source.
--
-- 'disableUseAsImportedSource', 'dataSetUsageConfiguration_disableUseAsImportedSource' - An option that controls whether a child dataset that\'s stored in
-- QuickSight can use this dataset as a source.
newDataSetUsageConfiguration ::
  DataSetUsageConfiguration
newDataSetUsageConfiguration =
  DataSetUsageConfiguration'
    { disableUseAsDirectQuerySource =
        Prelude.Nothing,
      disableUseAsImportedSource = Prelude.Nothing
    }

-- | An option that controls whether a child dataset of a direct query can
-- use this dataset as a source.
dataSetUsageConfiguration_disableUseAsDirectQuerySource :: Lens.Lens' DataSetUsageConfiguration (Prelude.Maybe Prelude.Bool)
dataSetUsageConfiguration_disableUseAsDirectQuerySource = Lens.lens (\DataSetUsageConfiguration' {disableUseAsDirectQuerySource} -> disableUseAsDirectQuerySource) (\s@DataSetUsageConfiguration' {} a -> s {disableUseAsDirectQuerySource = a} :: DataSetUsageConfiguration)

-- | An option that controls whether a child dataset that\'s stored in
-- QuickSight can use this dataset as a source.
dataSetUsageConfiguration_disableUseAsImportedSource :: Lens.Lens' DataSetUsageConfiguration (Prelude.Maybe Prelude.Bool)
dataSetUsageConfiguration_disableUseAsImportedSource = Lens.lens (\DataSetUsageConfiguration' {disableUseAsImportedSource} -> disableUseAsImportedSource) (\s@DataSetUsageConfiguration' {} a -> s {disableUseAsImportedSource = a} :: DataSetUsageConfiguration)

instance Data.FromJSON DataSetUsageConfiguration where
  parseJSON =
    Data.withObject
      "DataSetUsageConfiguration"
      ( \x ->
          DataSetUsageConfiguration'
            Prelude.<$> (x Data..:? "DisableUseAsDirectQuerySource")
            Prelude.<*> (x Data..:? "DisableUseAsImportedSource")
      )

instance Prelude.Hashable DataSetUsageConfiguration where
  hashWithSalt _salt DataSetUsageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` disableUseAsDirectQuerySource
      `Prelude.hashWithSalt` disableUseAsImportedSource

instance Prelude.NFData DataSetUsageConfiguration where
  rnf DataSetUsageConfiguration' {..} =
    Prelude.rnf disableUseAsDirectQuerySource
      `Prelude.seq` Prelude.rnf disableUseAsImportedSource

instance Data.ToJSON DataSetUsageConfiguration where
  toJSON DataSetUsageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisableUseAsDirectQuerySource" Data..=)
              Prelude.<$> disableUseAsDirectQuerySource,
            ("DisableUseAsImportedSource" Data..=)
              Prelude.<$> disableUseAsImportedSource
          ]
      )
