{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LayerFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerFailure
  ( LayerFailure (..),

    -- * Smart constructor
    mkLayerFailure,

    -- * Lenses
    lfFailureReason,
    lfFailureCode,
    lfLayerDigest,
  )
where

import Network.AWS.ECR.Types.LayerFailureCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an Amazon ECR image layer failure.
--
-- /See:/ 'mkLayerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { failureReason ::
      Lude.Maybe Lude.Text,
    failureCode :: Lude.Maybe LayerFailureCode,
    layerDigest :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LayerFailure' with the minimum fields required to make a request.
--
-- * 'failureCode' - The failure code associated with the failure.
-- * 'failureReason' - The reason for the failure.
-- * 'layerDigest' - The layer digest associated with the failure.
mkLayerFailure ::
  LayerFailure
mkLayerFailure =
  LayerFailure'
    { failureReason = Lude.Nothing,
      failureCode = Lude.Nothing,
      layerDigest = Lude.Nothing
    }

-- | The reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFailureReason :: Lens.Lens' LayerFailure (Lude.Maybe Lude.Text)
lfFailureReason = Lens.lens (failureReason :: LayerFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: LayerFailure)
{-# DEPRECATED lfFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The failure code associated with the failure.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFailureCode :: Lens.Lens' LayerFailure (Lude.Maybe LayerFailureCode)
lfFailureCode = Lens.lens (failureCode :: LayerFailure -> Lude.Maybe LayerFailureCode) (\s a -> s {failureCode = a} :: LayerFailure)
{-# DEPRECATED lfFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The layer digest associated with the failure.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLayerDigest :: Lens.Lens' LayerFailure (Lude.Maybe Lude.Text)
lfLayerDigest = Lens.lens (layerDigest :: LayerFailure -> Lude.Maybe Lude.Text) (\s a -> s {layerDigest = a} :: LayerFailure)
{-# DEPRECATED lfLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

instance Lude.FromJSON LayerFailure where
  parseJSON =
    Lude.withObject
      "LayerFailure"
      ( \x ->
          LayerFailure'
            Lude.<$> (x Lude..:? "failureReason")
            Lude.<*> (x Lude..:? "failureCode")
            Lude.<*> (x Lude..:? "layerDigest")
      )
