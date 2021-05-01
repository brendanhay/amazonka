{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.Types.ChefConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ChefConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Chef configuration.
--
-- /See:/ 'newChefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { -- | Whether to enable Berkshelf.
    manageBerkshelf :: Prelude.Maybe Prelude.Bool,
    -- | The Berkshelf version.
    berkshelfVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChefConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manageBerkshelf', 'chefConfiguration_manageBerkshelf' - Whether to enable Berkshelf.
--
-- 'berkshelfVersion', 'chefConfiguration_berkshelfVersion' - The Berkshelf version.
newChefConfiguration ::
  ChefConfiguration
newChefConfiguration =
  ChefConfiguration'
    { manageBerkshelf =
        Prelude.Nothing,
      berkshelfVersion = Prelude.Nothing
    }

-- | Whether to enable Berkshelf.
chefConfiguration_manageBerkshelf :: Lens.Lens' ChefConfiguration (Prelude.Maybe Prelude.Bool)
chefConfiguration_manageBerkshelf = Lens.lens (\ChefConfiguration' {manageBerkshelf} -> manageBerkshelf) (\s@ChefConfiguration' {} a -> s {manageBerkshelf = a} :: ChefConfiguration)

-- | The Berkshelf version.
chefConfiguration_berkshelfVersion :: Lens.Lens' ChefConfiguration (Prelude.Maybe Prelude.Text)
chefConfiguration_berkshelfVersion = Lens.lens (\ChefConfiguration' {berkshelfVersion} -> berkshelfVersion) (\s@ChefConfiguration' {} a -> s {berkshelfVersion = a} :: ChefConfiguration)

instance Prelude.FromJSON ChefConfiguration where
  parseJSON =
    Prelude.withObject
      "ChefConfiguration"
      ( \x ->
          ChefConfiguration'
            Prelude.<$> (x Prelude..:? "ManageBerkshelf")
            Prelude.<*> (x Prelude..:? "BerkshelfVersion")
      )

instance Prelude.Hashable ChefConfiguration

instance Prelude.NFData ChefConfiguration

instance Prelude.ToJSON ChefConfiguration where
  toJSON ChefConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ManageBerkshelf" Prelude..=)
              Prelude.<$> manageBerkshelf,
            ("BerkshelfVersion" Prelude..=)
              Prelude.<$> berkshelfVersion
          ]
      )
