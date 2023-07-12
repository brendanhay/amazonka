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
-- Module      : Amazonka.QuickSight.Types.TableFieldURLConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldURLConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldImageConfiguration
import Amazonka.QuickSight.Types.TableFieldLinkConfiguration

-- | The URL configuration for a table field.
--
-- /See:/ 'newTableFieldURLConfiguration' smart constructor.
data TableFieldURLConfiguration = TableFieldURLConfiguration'
  { -- | The image configuration of a table field URL.
    imageConfiguration :: Prelude.Maybe TableFieldImageConfiguration,
    -- | The link configuration of a table field URL.
    linkConfiguration :: Prelude.Maybe TableFieldLinkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldURLConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfiguration', 'tableFieldURLConfiguration_imageConfiguration' - The image configuration of a table field URL.
--
-- 'linkConfiguration', 'tableFieldURLConfiguration_linkConfiguration' - The link configuration of a table field URL.
newTableFieldURLConfiguration ::
  TableFieldURLConfiguration
newTableFieldURLConfiguration =
  TableFieldURLConfiguration'
    { imageConfiguration =
        Prelude.Nothing,
      linkConfiguration = Prelude.Nothing
    }

-- | The image configuration of a table field URL.
tableFieldURLConfiguration_imageConfiguration :: Lens.Lens' TableFieldURLConfiguration (Prelude.Maybe TableFieldImageConfiguration)
tableFieldURLConfiguration_imageConfiguration = Lens.lens (\TableFieldURLConfiguration' {imageConfiguration} -> imageConfiguration) (\s@TableFieldURLConfiguration' {} a -> s {imageConfiguration = a} :: TableFieldURLConfiguration)

-- | The link configuration of a table field URL.
tableFieldURLConfiguration_linkConfiguration :: Lens.Lens' TableFieldURLConfiguration (Prelude.Maybe TableFieldLinkConfiguration)
tableFieldURLConfiguration_linkConfiguration = Lens.lens (\TableFieldURLConfiguration' {linkConfiguration} -> linkConfiguration) (\s@TableFieldURLConfiguration' {} a -> s {linkConfiguration = a} :: TableFieldURLConfiguration)

instance Data.FromJSON TableFieldURLConfiguration where
  parseJSON =
    Data.withObject
      "TableFieldURLConfiguration"
      ( \x ->
          TableFieldURLConfiguration'
            Prelude.<$> (x Data..:? "ImageConfiguration")
            Prelude.<*> (x Data..:? "LinkConfiguration")
      )

instance Prelude.Hashable TableFieldURLConfiguration where
  hashWithSalt _salt TableFieldURLConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` imageConfiguration
      `Prelude.hashWithSalt` linkConfiguration

instance Prelude.NFData TableFieldURLConfiguration where
  rnf TableFieldURLConfiguration' {..} =
    Prelude.rnf imageConfiguration
      `Prelude.seq` Prelude.rnf linkConfiguration

instance Data.ToJSON TableFieldURLConfiguration where
  toJSON TableFieldURLConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImageConfiguration" Data..=)
              Prelude.<$> imageConfiguration,
            ("LinkConfiguration" Data..=)
              Prelude.<$> linkConfiguration
          ]
      )
