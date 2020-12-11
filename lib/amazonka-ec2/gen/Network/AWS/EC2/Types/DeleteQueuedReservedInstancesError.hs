-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
  ( DeleteQueuedReservedInstancesError (..),

    -- * Smart constructor
    mkDeleteQueuedReservedInstancesError,

    -- * Lenses
    dqrieCode,
    dqrieMessage,
  )
where

import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the error for a Reserved Instance whose queued purchase could not be deleted.
--
-- /See:/ 'mkDeleteQueuedReservedInstancesError' smart constructor.
data DeleteQueuedReservedInstancesError = DeleteQueuedReservedInstancesError'
  { code ::
      Lude.Maybe
        DeleteQueuedReservedInstancesErrorCode,
    message ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueuedReservedInstancesError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message.
mkDeleteQueuedReservedInstancesError ::
  DeleteQueuedReservedInstancesError
mkDeleteQueuedReservedInstancesError =
  DeleteQueuedReservedInstancesError'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrieCode :: Lens.Lens' DeleteQueuedReservedInstancesError (Lude.Maybe DeleteQueuedReservedInstancesErrorCode)
dqrieCode = Lens.lens (code :: DeleteQueuedReservedInstancesError -> Lude.Maybe DeleteQueuedReservedInstancesErrorCode) (\s a -> s {code = a} :: DeleteQueuedReservedInstancesError)
{-# DEPRECATED dqrieCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrieMessage :: Lens.Lens' DeleteQueuedReservedInstancesError (Lude.Maybe Lude.Text)
dqrieMessage = Lens.lens (message :: DeleteQueuedReservedInstancesError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DeleteQueuedReservedInstancesError)
{-# DEPRECATED dqrieMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML DeleteQueuedReservedInstancesError where
  parseXML x =
    DeleteQueuedReservedInstancesError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
