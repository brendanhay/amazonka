{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentTypeInfo
  ( SegmentTypeInfo (..),

    -- * Smart constructor
    mkSegmentTypeInfo,

    -- * Lenses
    stiModelVersion,
    stiType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.SegmentType

-- | Information about the type of a segment requested in a call to 'StartSegmentDetection' . An array of @SegmentTypeInfo@ objects is returned by the response from 'GetSegmentDetection' .
--
-- /See:/ 'mkSegmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { modelVersion ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe SegmentType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentTypeInfo' with the minimum fields required to make a request.
--
-- * 'modelVersion' - The version of the model used to detect segments.
-- * 'type'' - The type of a segment (technical cue or shot detection).
mkSegmentTypeInfo ::
  SegmentTypeInfo
mkSegmentTypeInfo =
  SegmentTypeInfo'
    { modelVersion = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The version of the model used to detect segments.
--
-- /Note:/ Consider using 'modelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiModelVersion :: Lens.Lens' SegmentTypeInfo (Lude.Maybe Lude.Text)
stiModelVersion = Lens.lens (modelVersion :: SegmentTypeInfo -> Lude.Maybe Lude.Text) (\s a -> s {modelVersion = a} :: SegmentTypeInfo)
{-# DEPRECATED stiModelVersion "Use generic-lens or generic-optics with 'modelVersion' instead." #-}

-- | The type of a segment (technical cue or shot detection).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stiType :: Lens.Lens' SegmentTypeInfo (Lude.Maybe SegmentType)
stiType = Lens.lens (type' :: SegmentTypeInfo -> Lude.Maybe SegmentType) (\s a -> s {type' = a} :: SegmentTypeInfo)
{-# DEPRECATED stiType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SegmentTypeInfo where
  parseJSON =
    Lude.withObject
      "SegmentTypeInfo"
      ( \x ->
          SegmentTypeInfo'
            Lude.<$> (x Lude..:? "ModelVersion") Lude.<*> (x Lude..:? "Type")
      )
