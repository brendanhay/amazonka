-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockLegalHold
  ( ObjectLockLegalHold (..),

    -- * Smart constructor
    mkObjectLockLegalHold,

    -- * Lenses
    ollhStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockLegalHoldStatus

-- | A Legal Hold configuration for an object.
--
-- /See:/ 'mkObjectLockLegalHold' smart constructor.
newtype ObjectLockLegalHold = ObjectLockLegalHold'
  { status ::
      Lude.Maybe ObjectLockLegalHoldStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectLockLegalHold' with the minimum fields required to make a request.
--
-- * 'status' - Indicates whether the specified object has a Legal Hold in place.
mkObjectLockLegalHold ::
  ObjectLockLegalHold
mkObjectLockLegalHold = ObjectLockLegalHold' {status = Lude.Nothing}

-- | Indicates whether the specified object has a Legal Hold in place.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ollhStatus :: Lens.Lens' ObjectLockLegalHold (Lude.Maybe ObjectLockLegalHoldStatus)
ollhStatus = Lens.lens (status :: ObjectLockLegalHold -> Lude.Maybe ObjectLockLegalHoldStatus) (\s a -> s {status = a} :: ObjectLockLegalHold)
{-# DEPRECATED ollhStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML ObjectLockLegalHold where
  parseXML x = ObjectLockLegalHold' Lude.<$> (x Lude..@? "Status")

instance Lude.ToXML ObjectLockLegalHold where
  toXML ObjectLockLegalHold' {..} =
    Lude.mconcat ["Status" Lude.@= status]
