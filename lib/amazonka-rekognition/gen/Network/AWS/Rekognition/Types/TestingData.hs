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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for testing. Optionally, if @AutoCreate@ is set, Amazon Rekognition Custom Labels creates a testing dataset using an 80/20 split of the training dataset.
--
-- /See:/ 'mkTestingData' smart constructor.
data TestingData = TestingData'
  { assets :: Lude.Maybe [Asset],
    autoCreate :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestingData' with the minimum fields required to make a request.
--
-- * 'assets' - The assets used for testing.
-- * 'autoCreate' - If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
mkTestingData ::
  TestingData
mkTestingData =
  TestingData' {assets = Lude.Nothing, autoCreate = Lude.Nothing}

-- | The assets used for testing.
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAssets :: Lens.Lens' TestingData (Lude.Maybe [Asset])
tdAssets = Lens.lens (assets :: TestingData -> Lude.Maybe [Asset]) (\s a -> s {assets = a} :: TestingData)
{-# DEPRECATED tdAssets "Use generic-lens or generic-optics with 'assets' instead." #-}

-- | If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
--
-- /Note:/ Consider using 'autoCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAutoCreate :: Lens.Lens' TestingData (Lude.Maybe Lude.Bool)
tdAutoCreate = Lens.lens (autoCreate :: TestingData -> Lude.Maybe Lude.Bool) (\s a -> s {autoCreate = a} :: TestingData)
{-# DEPRECATED tdAutoCreate "Use generic-lens or generic-optics with 'autoCreate' instead." #-}

instance Lude.FromJSON TestingData where
  parseJSON =
    Lude.withObject
      "TestingData"
      ( \x ->
          TestingData'
            Lude.<$> (x Lude..:? "Assets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AutoCreate")
      )

instance Lude.ToJSON TestingData where
  toJSON TestingData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Assets" Lude..=) Lude.<$> assets,
            ("AutoCreate" Lude..=) Lude.<$> autoCreate
          ]
      )
