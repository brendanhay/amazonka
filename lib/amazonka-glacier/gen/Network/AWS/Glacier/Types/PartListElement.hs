{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.PartListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.PartListElement
  ( PartListElement (..),

    -- * Smart constructor
    mkPartListElement,

    -- * Lenses
    pleSHA256TreeHash,
    pleRangeInBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of the part sizes of the multipart upload.
--
-- /See:/ 'mkPartListElement' smart constructor.
data PartListElement = PartListElement'
  { -- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
    sHA256TreeHash :: Lude.Maybe Lude.Text,
    -- | The byte range of a part, inclusive of the upper value of the range.
    rangeInBytes :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartListElement' with the minimum fields required to make a request.
--
-- * 'sHA256TreeHash' - The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
-- * 'rangeInBytes' - The byte range of a part, inclusive of the upper value of the range.
mkPartListElement ::
  PartListElement
mkPartListElement =
  PartListElement'
    { sHA256TreeHash = Lude.Nothing,
      rangeInBytes = Lude.Nothing
    }

-- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
--
-- /Note:/ Consider using 'sHA256TreeHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleSHA256TreeHash :: Lens.Lens' PartListElement (Lude.Maybe Lude.Text)
pleSHA256TreeHash = Lens.lens (sHA256TreeHash :: PartListElement -> Lude.Maybe Lude.Text) (\s a -> s {sHA256TreeHash = a} :: PartListElement)
{-# DEPRECATED pleSHA256TreeHash "Use generic-lens or generic-optics with 'sHA256TreeHash' instead." #-}

-- | The byte range of a part, inclusive of the upper value of the range.
--
-- /Note:/ Consider using 'rangeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleRangeInBytes :: Lens.Lens' PartListElement (Lude.Maybe Lude.Text)
pleRangeInBytes = Lens.lens (rangeInBytes :: PartListElement -> Lude.Maybe Lude.Text) (\s a -> s {rangeInBytes = a} :: PartListElement)
{-# DEPRECATED pleRangeInBytes "Use generic-lens or generic-optics with 'rangeInBytes' instead." #-}

instance Lude.FromJSON PartListElement where
  parseJSON =
    Lude.withObject
      "PartListElement"
      ( \x ->
          PartListElement'
            Lude.<$> (x Lude..:? "SHA256TreeHash") Lude.<*> (x Lude..:? "RangeInBytes")
      )
