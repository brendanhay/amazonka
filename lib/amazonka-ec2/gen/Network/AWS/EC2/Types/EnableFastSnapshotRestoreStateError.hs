{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
  ( EnableFastSnapshotRestoreStateError (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreStateError,

    -- * Lenses
    efsrseCode,
    efsrseMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreStateError' smart constructor.
data EnableFastSnapshotRestoreStateError = EnableFastSnapshotRestoreStateError'
  { code ::
      Lude.Maybe
        Lude.Text,
    message ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableFastSnapshotRestoreStateError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message.
mkEnableFastSnapshotRestoreStateError ::
  EnableFastSnapshotRestoreStateError
mkEnableFastSnapshotRestoreStateError =
  EnableFastSnapshotRestoreStateError'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseCode :: Lens.Lens' EnableFastSnapshotRestoreStateError (Lude.Maybe Lude.Text)
efsrseCode = Lens.lens (code :: EnableFastSnapshotRestoreStateError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: EnableFastSnapshotRestoreStateError)
{-# DEPRECATED efsrseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseMessage :: Lens.Lens' EnableFastSnapshotRestoreStateError (Lude.Maybe Lude.Text)
efsrseMessage = Lens.lens (message :: EnableFastSnapshotRestoreStateError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EnableFastSnapshotRestoreStateError)
{-# DEPRECATED efsrseMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML EnableFastSnapshotRestoreStateError where
  parseXML x =
    EnableFastSnapshotRestoreStateError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
