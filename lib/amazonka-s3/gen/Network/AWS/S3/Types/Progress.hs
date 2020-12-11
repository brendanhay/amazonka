-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Progress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Progress
  ( Progress (..),

    -- * Smart constructor
    mkProgress,

    -- * Lenses
    pBytesReturned,
    pBytesScanned,
    pBytesProcessed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | This data type contains information about progress of an operation.
--
-- /See:/ 'mkProgress' smart constructor.
data Progress = Progress'
  { bytesReturned :: Lude.Maybe Lude.Integer,
    bytesScanned :: Lude.Maybe Lude.Integer,
    bytesProcessed :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Progress' with the minimum fields required to make a request.
--
-- * 'bytesProcessed' - The current number of uncompressed object bytes processed.
-- * 'bytesReturned' - The current number of bytes of records payload data returned.
-- * 'bytesScanned' - The current number of object bytes scanned.
mkProgress ::
  Progress
mkProgress =
  Progress'
    { bytesReturned = Lude.Nothing,
      bytesScanned = Lude.Nothing,
      bytesProcessed = Lude.Nothing
    }

-- | The current number of bytes of records payload data returned.
--
-- /Note:/ Consider using 'bytesReturned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesReturned :: Lens.Lens' Progress (Lude.Maybe Lude.Integer)
pBytesReturned = Lens.lens (bytesReturned :: Progress -> Lude.Maybe Lude.Integer) (\s a -> s {bytesReturned = a} :: Progress)
{-# DEPRECATED pBytesReturned "Use generic-lens or generic-optics with 'bytesReturned' instead." #-}

-- | The current number of object bytes scanned.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesScanned :: Lens.Lens' Progress (Lude.Maybe Lude.Integer)
pBytesScanned = Lens.lens (bytesScanned :: Progress -> Lude.Maybe Lude.Integer) (\s a -> s {bytesScanned = a} :: Progress)
{-# DEPRECATED pBytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead." #-}

-- | The current number of uncompressed object bytes processed.
--
-- /Note:/ Consider using 'bytesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesProcessed :: Lens.Lens' Progress (Lude.Maybe Lude.Integer)
pBytesProcessed = Lens.lens (bytesProcessed :: Progress -> Lude.Maybe Lude.Integer) (\s a -> s {bytesProcessed = a} :: Progress)
{-# DEPRECATED pBytesProcessed "Use generic-lens or generic-optics with 'bytesProcessed' instead." #-}

instance Lude.FromXML Progress where
  parseXML x =
    Progress'
      Lude.<$> (x Lude..@? "BytesReturned")
      Lude.<*> (x Lude..@? "BytesScanned")
      Lude.<*> (x Lude..@? "BytesProcessed")
