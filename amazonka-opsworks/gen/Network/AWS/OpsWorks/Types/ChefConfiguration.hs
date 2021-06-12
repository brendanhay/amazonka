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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the Chef configuration.
--
-- /See:/ 'newChefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { -- | Whether to enable Berkshelf.
    manageBerkshelf :: Core.Maybe Core.Bool,
    -- | The Berkshelf version.
    berkshelfVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { manageBerkshelf = Core.Nothing,
      berkshelfVersion = Core.Nothing
    }

-- | Whether to enable Berkshelf.
chefConfiguration_manageBerkshelf :: Lens.Lens' ChefConfiguration (Core.Maybe Core.Bool)
chefConfiguration_manageBerkshelf = Lens.lens (\ChefConfiguration' {manageBerkshelf} -> manageBerkshelf) (\s@ChefConfiguration' {} a -> s {manageBerkshelf = a} :: ChefConfiguration)

-- | The Berkshelf version.
chefConfiguration_berkshelfVersion :: Lens.Lens' ChefConfiguration (Core.Maybe Core.Text)
chefConfiguration_berkshelfVersion = Lens.lens (\ChefConfiguration' {berkshelfVersion} -> berkshelfVersion) (\s@ChefConfiguration' {} a -> s {berkshelfVersion = a} :: ChefConfiguration)

instance Core.FromJSON ChefConfiguration where
  parseJSON =
    Core.withObject
      "ChefConfiguration"
      ( \x ->
          ChefConfiguration'
            Core.<$> (x Core..:? "ManageBerkshelf")
            Core.<*> (x Core..:? "BerkshelfVersion")
      )

instance Core.Hashable ChefConfiguration

instance Core.NFData ChefConfiguration

instance Core.ToJSON ChefConfiguration where
  toJSON ChefConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ManageBerkshelf" Core..=)
              Core.<$> manageBerkshelf,
            ("BerkshelfVersion" Core..=)
              Core.<$> berkshelfVersion
          ]
      )
