{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Part
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Part
  ( Part (..),

    -- * Smart constructor
    mkPart,

    -- * Lenses
    pETag,
    pSize,
    pPartNumber,
    pLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for elements related to a part.
--
-- /See:/ 'mkPart' smart constructor.
data Part = Part'
  { eTag :: Lude.Maybe ETag,
    size :: Lude.Maybe Lude.Int,
    partNumber :: Lude.Maybe Lude.Int,
    lastModified :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Part' with the minimum fields required to make a request.
--
-- * 'eTag' - Entity tag returned when the part was uploaded.
-- * 'lastModified' - Date and time at which the part was uploaded.
-- * 'partNumber' - Part number identifying the part. This is a positive integer between 1 and 10,000.
-- * 'size' - Size in bytes of the uploaded part data.
mkPart ::
  Part
mkPart =
  Part'
    { eTag = Lude.Nothing,
      size = Lude.Nothing,
      partNumber = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | Entity tag returned when the part was uploaded.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pETag :: Lens.Lens' Part (Lude.Maybe ETag)
pETag = Lens.lens (eTag :: Part -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: Part)
{-# DEPRECATED pETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Size in bytes of the uploaded part data.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSize :: Lens.Lens' Part (Lude.Maybe Lude.Int)
pSize = Lens.lens (size :: Part -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: Part)
{-# DEPRECATED pSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Part number identifying the part. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPartNumber :: Lens.Lens' Part (Lude.Maybe Lude.Int)
pPartNumber = Lens.lens (partNumber :: Part -> Lude.Maybe Lude.Int) (\s a -> s {partNumber = a} :: Part)
{-# DEPRECATED pPartNumber "Use generic-lens or generic-optics with 'partNumber' instead." #-}

-- | Date and time at which the part was uploaded.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastModified :: Lens.Lens' Part (Lude.Maybe Lude.DateTime)
pLastModified = Lens.lens (lastModified :: Part -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: Part)
{-# DEPRECATED pLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML Part where
  parseXML x =
    Part'
      Lude.<$> (x Lude..@? "ETag")
      Lude.<*> (x Lude..@? "Size")
      Lude.<*> (x Lude..@? "PartNumber")
      Lude.<*> (x Lude..@? "LastModified")
