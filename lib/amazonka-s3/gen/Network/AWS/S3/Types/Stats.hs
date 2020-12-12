{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Stats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Stats
  ( Stats (..),

    -- * Smart constructor
    mkStats,

    -- * Lenses
    sBytesReturned,
    sBytesScanned,
    sBytesProcessed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for the stats details.
--
-- /See:/ 'mkStats' smart constructor.
data Stats = Stats'
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

-- | Creates a value of 'Stats' with the minimum fields required to make a request.
--
-- * 'bytesProcessed' - The total number of uncompressed object bytes processed.
-- * 'bytesReturned' - The total number of bytes of records payload data returned.
-- * 'bytesScanned' - The total number of object bytes scanned.
mkStats ::
  Stats
mkStats =
  Stats'
    { bytesReturned = Lude.Nothing,
      bytesScanned = Lude.Nothing,
      bytesProcessed = Lude.Nothing
    }

-- | The total number of bytes of records payload data returned.
--
-- /Note:/ Consider using 'bytesReturned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesReturned :: Lens.Lens' Stats (Lude.Maybe Lude.Integer)
sBytesReturned = Lens.lens (bytesReturned :: Stats -> Lude.Maybe Lude.Integer) (\s a -> s {bytesReturned = a} :: Stats)
{-# DEPRECATED sBytesReturned "Use generic-lens or generic-optics with 'bytesReturned' instead." #-}

-- | The total number of object bytes scanned.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesScanned :: Lens.Lens' Stats (Lude.Maybe Lude.Integer)
sBytesScanned = Lens.lens (bytesScanned :: Stats -> Lude.Maybe Lude.Integer) (\s a -> s {bytesScanned = a} :: Stats)
{-# DEPRECATED sBytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead." #-}

-- | The total number of uncompressed object bytes processed.
--
-- /Note:/ Consider using 'bytesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesProcessed :: Lens.Lens' Stats (Lude.Maybe Lude.Integer)
sBytesProcessed = Lens.lens (bytesProcessed :: Stats -> Lude.Maybe Lude.Integer) (\s a -> s {bytesProcessed = a} :: Stats)
{-# DEPRECATED sBytesProcessed "Use generic-lens or generic-optics with 'bytesProcessed' instead." #-}

instance Lude.FromXML Stats where
  parseXML x =
    Stats'
      Lude.<$> (x Lude..@? "BytesReturned")
      Lude.<*> (x Lude..@? "BytesScanned")
      Lude.<*> (x Lude..@? "BytesProcessed")
