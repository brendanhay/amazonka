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
-- Module      : Amazonka.OpsWorks.Types.ChefConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ChefConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Chef configuration.
--
-- /See:/ 'newChefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { -- | The Berkshelf version.
    berkshelfVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable Berkshelf.
    manageBerkshelf :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChefConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'berkshelfVersion', 'chefConfiguration_berkshelfVersion' - The Berkshelf version.
--
-- 'manageBerkshelf', 'chefConfiguration_manageBerkshelf' - Whether to enable Berkshelf.
newChefConfiguration ::
  ChefConfiguration
newChefConfiguration =
  ChefConfiguration'
    { berkshelfVersion =
        Prelude.Nothing,
      manageBerkshelf = Prelude.Nothing
    }

-- | The Berkshelf version.
chefConfiguration_berkshelfVersion :: Lens.Lens' ChefConfiguration (Prelude.Maybe Prelude.Text)
chefConfiguration_berkshelfVersion = Lens.lens (\ChefConfiguration' {berkshelfVersion} -> berkshelfVersion) (\s@ChefConfiguration' {} a -> s {berkshelfVersion = a} :: ChefConfiguration)

-- | Whether to enable Berkshelf.
chefConfiguration_manageBerkshelf :: Lens.Lens' ChefConfiguration (Prelude.Maybe Prelude.Bool)
chefConfiguration_manageBerkshelf = Lens.lens (\ChefConfiguration' {manageBerkshelf} -> manageBerkshelf) (\s@ChefConfiguration' {} a -> s {manageBerkshelf = a} :: ChefConfiguration)

instance Data.FromJSON ChefConfiguration where
  parseJSON =
    Data.withObject
      "ChefConfiguration"
      ( \x ->
          ChefConfiguration'
            Prelude.<$> (x Data..:? "BerkshelfVersion")
            Prelude.<*> (x Data..:? "ManageBerkshelf")
      )

instance Prelude.Hashable ChefConfiguration where
  hashWithSalt _salt ChefConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` berkshelfVersion
      `Prelude.hashWithSalt` manageBerkshelf

instance Prelude.NFData ChefConfiguration where
  rnf ChefConfiguration' {..} =
    Prelude.rnf berkshelfVersion
      `Prelude.seq` Prelude.rnf manageBerkshelf

instance Data.ToJSON ChefConfiguration where
  toJSON ChefConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BerkshelfVersion" Data..=)
              Prelude.<$> berkshelfVersion,
            ("ManageBerkshelf" Data..=)
              Prelude.<$> manageBerkshelf
          ]
      )
