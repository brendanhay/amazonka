{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Asset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Asset
  ( Asset (..),

    -- * Smart constructor
    mkAsset,

    -- * Lenses
    aGroundTruthManifest,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.GroundTruthManifest

-- | Assets are the images that you use to train and evaluate a model version. Assets can also contain validation information that you use to debug a failed model training.
--
-- /See:/ 'mkAsset' smart constructor.
newtype Asset = Asset'
  { groundTruthManifest ::
      Lude.Maybe GroundTruthManifest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Asset' with the minimum fields required to make a request.
--
-- * 'groundTruthManifest' - Undocumented field.
mkAsset ::
  Asset
mkAsset = Asset' {groundTruthManifest = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'groundTruthManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aGroundTruthManifest :: Lens.Lens' Asset (Lude.Maybe GroundTruthManifest)
aGroundTruthManifest = Lens.lens (groundTruthManifest :: Asset -> Lude.Maybe GroundTruthManifest) (\s a -> s {groundTruthManifest = a} :: Asset)
{-# DEPRECATED aGroundTruthManifest "Use generic-lens or generic-optics with 'groundTruthManifest' instead." #-}

instance Lude.FromJSON Asset where
  parseJSON =
    Lude.withObject
      "Asset"
      (\x -> Asset' Lude.<$> (x Lude..:? "GroundTruthManifest"))

instance Lude.ToJSON Asset where
  toJSON Asset' {..} =
    Lude.object
      ( Lude.catMaybes
          [("GroundTruthManifest" Lude..=) Lude.<$> groundTruthManifest]
      )
