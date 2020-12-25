{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TestingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TestingData
  ( TestingData (..),

    -- * Smart constructor
    mkTestingData,

    -- * Lenses
    tdAssets,
    tdAutoCreate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Asset as Types

-- | The dataset used for testing. Optionally, if @AutoCreate@ is set, Amazon Rekognition Custom Labels creates a testing dataset using an 80/20 split of the training dataset.
--
-- /See:/ 'mkTestingData' smart constructor.
data TestingData = TestingData'
  { -- | The assets used for testing.
    assets :: Core.Maybe [Types.Asset],
    -- | If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
    autoCreate :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestingData' value with any optional fields omitted.
mkTestingData ::
  TestingData
mkTestingData =
  TestingData' {assets = Core.Nothing, autoCreate = Core.Nothing}

-- | The assets used for testing.
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAssets :: Lens.Lens' TestingData (Core.Maybe [Types.Asset])
tdAssets = Lens.field @"assets"
{-# DEPRECATED tdAssets "Use generic-lens or generic-optics with 'assets' instead." #-}

-- | If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
--
-- /Note:/ Consider using 'autoCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAutoCreate :: Lens.Lens' TestingData (Core.Maybe Core.Bool)
tdAutoCreate = Lens.field @"autoCreate"
{-# DEPRECATED tdAutoCreate "Use generic-lens or generic-optics with 'autoCreate' instead." #-}

instance Core.FromJSON TestingData where
  toJSON TestingData {..} =
    Core.object
      ( Core.catMaybes
          [ ("Assets" Core..=) Core.<$> assets,
            ("AutoCreate" Core..=) Core.<$> autoCreate
          ]
      )

instance Core.FromJSON TestingData where
  parseJSON =
    Core.withObject "TestingData" Core.$
      \x ->
        TestingData'
          Core.<$> (x Core..:? "Assets") Core.<*> (x Core..:? "AutoCreate")
