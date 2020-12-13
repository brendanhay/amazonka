{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyPartResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyPartResult
  ( CopyPartResult (..),

    -- * Smart constructor
    mkCopyPartResult,

    -- * Lenses
    cprETag,
    cprLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
-- /See:/ 'mkCopyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { -- | Entity tag of the object.
    eTag :: Lude.Maybe ETag,
    -- | Date and time at which the object was uploaded.
    lastModified :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyPartResult' with the minimum fields required to make a request.
--
-- * 'eTag' - Entity tag of the object.
-- * 'lastModified' - Date and time at which the object was uploaded.
mkCopyPartResult ::
  CopyPartResult
mkCopyPartResult =
  CopyPartResult' {eTag = Lude.Nothing, lastModified = Lude.Nothing}

-- | Entity tag of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprETag :: Lens.Lens' CopyPartResult (Lude.Maybe ETag)
cprETag = Lens.lens (eTag :: CopyPartResult -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: CopyPartResult)
{-# DEPRECATED cprETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Date and time at which the object was uploaded.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprLastModified :: Lens.Lens' CopyPartResult (Lude.Maybe Lude.DateTime)
cprLastModified = Lens.lens (lastModified :: CopyPartResult -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: CopyPartResult)
{-# DEPRECATED cprLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML CopyPartResult where
  parseXML x =
    CopyPartResult'
      Lude.<$> (x Lude..@? "ETag") Lude.<*> (x Lude..@? "LastModified")
