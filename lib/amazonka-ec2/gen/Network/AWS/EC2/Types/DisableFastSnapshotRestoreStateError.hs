{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
  ( DisableFastSnapshotRestoreStateError (..),

    -- * Smart constructor
    mkDisableFastSnapshotRestoreStateError,

    -- * Lenses
    dfsrseCode,
    dfsrseMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an error that occurred when disabling fast snapshot restores.
--
-- /See:/ 'mkDisableFastSnapshotRestoreStateError' smart constructor.
data DisableFastSnapshotRestoreStateError = DisableFastSnapshotRestoreStateError'
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

-- | Creates a value of 'DisableFastSnapshotRestoreStateError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message.
mkDisableFastSnapshotRestoreStateError ::
  DisableFastSnapshotRestoreStateError
mkDisableFastSnapshotRestoreStateError =
  DisableFastSnapshotRestoreStateError'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseCode :: Lens.Lens' DisableFastSnapshotRestoreStateError (Lude.Maybe Lude.Text)
dfsrseCode = Lens.lens (code :: DisableFastSnapshotRestoreStateError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: DisableFastSnapshotRestoreStateError)
{-# DEPRECATED dfsrseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseMessage :: Lens.Lens' DisableFastSnapshotRestoreStateError (Lude.Maybe Lude.Text)
dfsrseMessage = Lens.lens (message :: DisableFastSnapshotRestoreStateError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DisableFastSnapshotRestoreStateError)
{-# DEPRECATED dfsrseMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML DisableFastSnapshotRestoreStateError where
  parseXML x =
    DisableFastSnapshotRestoreStateError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
