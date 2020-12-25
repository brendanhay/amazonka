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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreStateError' smart constructor.
data EnableFastSnapshotRestoreStateError = EnableFastSnapshotRestoreStateError'
  { -- | The error code.
    code :: Core.Maybe Types.String,
    -- | The error message.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableFastSnapshotRestoreStateError' value with any optional fields omitted.
mkEnableFastSnapshotRestoreStateError ::
  EnableFastSnapshotRestoreStateError
mkEnableFastSnapshotRestoreStateError =
  EnableFastSnapshotRestoreStateError'
    { code = Core.Nothing,
      message = Core.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseCode :: Lens.Lens' EnableFastSnapshotRestoreStateError (Core.Maybe Types.String)
efsrseCode = Lens.field @"code"
{-# DEPRECATED efsrseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseMessage :: Lens.Lens' EnableFastSnapshotRestoreStateError (Core.Maybe Types.String)
efsrseMessage = Lens.field @"message"
{-# DEPRECATED efsrseMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML EnableFastSnapshotRestoreStateError where
  parseXML x =
    EnableFastSnapshotRestoreStateError'
      Core.<$> (x Core..@? "code") Core.<*> (x Core..@? "message")
