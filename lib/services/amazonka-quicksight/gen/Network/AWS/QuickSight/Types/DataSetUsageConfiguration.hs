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
-- Module      : Network.AWS.QuickSight.Types.DataSetUsageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSetUsageConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The usage configuration to apply to child datasets that reference this
-- dataset as a source.
--
-- /See:/ 'newDataSetUsageConfiguration' smart constructor.
data DataSetUsageConfiguration = DataSetUsageConfiguration'
  { -- | An option that controls whether a child dataset that\'s stored in
    -- QuickSight can use this dataset as a source.
    disableUseAsImportedSource :: Prelude.Maybe Prelude.Bool,
    -- | An option that controls whether a child dataset of a direct query can
    -- use this dataset as a source.
    disableUseAsDirectQuerySource :: Prelude.Maybe Prelude.Bool
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
-- 'disableUseAsImportedSource', 'dataSetUsageConfiguration_disableUseAsImportedSource' - An option that controls whether a child dataset that\'s stored in
-- QuickSight can use this dataset as a source.
--
-- 'disableUseAsDirectQuerySource', 'dataSetUsageConfiguration_disableUseAsDirectQuerySource' - An option that controls whether a child dataset of a direct query can
-- use this dataset as a source.
newDataSetUsageConfiguration ::
  DataSetUsageConfiguration
newDataSetUsageConfiguration =
  DataSetUsageConfiguration'
    { disableUseAsImportedSource =
        Prelude.Nothing,
      disableUseAsDirectQuerySource = Prelude.Nothing
    }

-- | An option that controls whether a child dataset that\'s stored in
-- QuickSight can use this dataset as a source.
dataSetUsageConfiguration_disableUseAsImportedSource :: Lens.Lens' DataSetUsageConfiguration (Prelude.Maybe Prelude.Bool)
dataSetUsageConfiguration_disableUseAsImportedSource = Lens.lens (\DataSetUsageConfiguration' {disableUseAsImportedSource} -> disableUseAsImportedSource) (\s@DataSetUsageConfiguration' {} a -> s {disableUseAsImportedSource = a} :: DataSetUsageConfiguration)

-- | An option that controls whether a child dataset of a direct query can
-- use this dataset as a source.
dataSetUsageConfiguration_disableUseAsDirectQuerySource :: Lens.Lens' DataSetUsageConfiguration (Prelude.Maybe Prelude.Bool)
dataSetUsageConfiguration_disableUseAsDirectQuerySource = Lens.lens (\DataSetUsageConfiguration' {disableUseAsDirectQuerySource} -> disableUseAsDirectQuerySource) (\s@DataSetUsageConfiguration' {} a -> s {disableUseAsDirectQuerySource = a} :: DataSetUsageConfiguration)

instance Core.FromJSON DataSetUsageConfiguration where
  parseJSON =
    Core.withObject
      "DataSetUsageConfiguration"
      ( \x ->
          DataSetUsageConfiguration'
            Prelude.<$> (x Core..:? "DisableUseAsImportedSource")
            Prelude.<*> (x Core..:? "DisableUseAsDirectQuerySource")
      )

instance Prelude.Hashable DataSetUsageConfiguration

instance Prelude.NFData DataSetUsageConfiguration

instance Core.ToJSON DataSetUsageConfiguration where
  toJSON DataSetUsageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisableUseAsImportedSource" Core..=)
              Prelude.<$> disableUseAsImportedSource,
            ("DisableUseAsDirectQuerySource" Core..=)
              Prelude.<$> disableUseAsDirectQuerySource
          ]
      )
