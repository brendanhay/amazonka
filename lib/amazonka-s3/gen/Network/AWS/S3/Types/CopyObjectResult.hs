{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyObjectResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyObjectResult
  ( CopyObjectResult (..),

    -- * Smart constructor
    mkCopyObjectResult,

    -- * Lenses
    corETag,
    corLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
-- /See:/ 'mkCopyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { eTag :: Lude.Maybe ETag,
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

-- | Creates a value of 'CopyObjectResult' with the minimum fields required to make a request.
--
-- * 'eTag' - Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
-- * 'lastModified' - Returns the date that the object was last modified.
mkCopyObjectResult ::
  CopyObjectResult
mkCopyObjectResult =
  CopyObjectResult'
    { eTag = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | Returns the ETag of the new object. The ETag reflects only changes to the contents of an object, not its metadata. The source and destination ETag is identical for a successfully copied object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corETag :: Lens.Lens' CopyObjectResult (Lude.Maybe ETag)
corETag = Lens.lens (eTag :: CopyObjectResult -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: CopyObjectResult)
{-# DEPRECATED corETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Returns the date that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corLastModified :: Lens.Lens' CopyObjectResult (Lude.Maybe Lude.DateTime)
corLastModified = Lens.lens (lastModified :: CopyObjectResult -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: CopyObjectResult)
{-# DEPRECATED corLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML CopyObjectResult where
  parseXML x =
    CopyObjectResult'
      Lude.<$> (x Lude..@? "ETag") Lude.<*> (x Lude..@? "LastModified")
