-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRetention
  ( ObjectLockRetention (..),

    -- * Smart constructor
    mkObjectLockRetention,

    -- * Lenses
    olrMode,
    olrRetainUntilDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | A Retention configuration for an object.
--
-- /See:/ 'mkObjectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { mode ::
      Lude.Maybe ObjectLockRetentionMode,
    retainUntilDate :: Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectLockRetention' with the minimum fields required to make a request.
--
-- * 'mode' - Indicates the Retention mode for the specified object.
-- * 'retainUntilDate' - The date on which this Object Lock Retention will expire.
mkObjectLockRetention ::
  ObjectLockRetention
mkObjectLockRetention =
  ObjectLockRetention'
    { mode = Lude.Nothing,
      retainUntilDate = Lude.Nothing
    }

-- | Indicates the Retention mode for the specified object.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrMode :: Lens.Lens' ObjectLockRetention (Lude.Maybe ObjectLockRetentionMode)
olrMode = Lens.lens (mode :: ObjectLockRetention -> Lude.Maybe ObjectLockRetentionMode) (\s a -> s {mode = a} :: ObjectLockRetention)
{-# DEPRECATED olrMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The date on which this Object Lock Retention will expire.
--
-- /Note:/ Consider using 'retainUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrRetainUntilDate :: Lens.Lens' ObjectLockRetention (Lude.Maybe Lude.ISO8601)
olrRetainUntilDate = Lens.lens (retainUntilDate :: ObjectLockRetention -> Lude.Maybe Lude.ISO8601) (\s a -> s {retainUntilDate = a} :: ObjectLockRetention)
{-# DEPRECATED olrRetainUntilDate "Use generic-lens or generic-optics with 'retainUntilDate' instead." #-}

instance Lude.FromXML ObjectLockRetention where
  parseXML x =
    ObjectLockRetention'
      Lude.<$> (x Lude..@? "Mode") Lude.<*> (x Lude..@? "RetainUntilDate")

instance Lude.ToXML ObjectLockRetention where
  toXML ObjectLockRetention' {..} =
    Lude.mconcat
      ["Mode" Lude.@= mode, "RetainUntilDate" Lude.@= retainUntilDate]
