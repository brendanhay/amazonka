{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
  ( FailedScheduledUpdateGroupActionRequest (..),

    -- * Smart constructor
    mkFailedScheduledUpdateGroupActionRequest,

    -- * Lenses
    fsugarScheduledActionName,
    fsugarErrorCode,
    fsugarErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a scheduled action that could not be created, updated, or deleted.
--
-- /See:/ 'mkFailedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { -- | The name of the scheduled action.
    scheduledActionName :: Lude.Text,
    -- | The error code.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The error message accompanying the error code.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedScheduledUpdateGroupActionRequest' with the minimum fields required to make a request.
--
-- * 'scheduledActionName' - The name of the scheduled action.
-- * 'errorCode' - The error code.
-- * 'errorMessage' - The error message accompanying the error code.
mkFailedScheduledUpdateGroupActionRequest ::
  -- | 'scheduledActionName'
  Lude.Text ->
  FailedScheduledUpdateGroupActionRequest
mkFailedScheduledUpdateGroupActionRequest pScheduledActionName_ =
  FailedScheduledUpdateGroupActionRequest'
    { scheduledActionName =
        pScheduledActionName_,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarScheduledActionName :: Lens.Lens' FailedScheduledUpdateGroupActionRequest Lude.Text
fsugarScheduledActionName = Lens.lens (scheduledActionName :: FailedScheduledUpdateGroupActionRequest -> Lude.Text) (\s a -> s {scheduledActionName = a} :: FailedScheduledUpdateGroupActionRequest)
{-# DEPRECATED fsugarScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorCode :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Text)
fsugarErrorCode = Lens.lens (errorCode :: FailedScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: FailedScheduledUpdateGroupActionRequest)
{-# DEPRECATED fsugarErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message accompanying the error code.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsugarErrorMessage :: Lens.Lens' FailedScheduledUpdateGroupActionRequest (Lude.Maybe Lude.Text)
fsugarErrorMessage = Lens.lens (errorMessage :: FailedScheduledUpdateGroupActionRequest -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailedScheduledUpdateGroupActionRequest)
{-# DEPRECATED fsugarErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromXML FailedScheduledUpdateGroupActionRequest where
  parseXML x =
    FailedScheduledUpdateGroupActionRequest'
      Lude.<$> (x Lude..@ "ScheduledActionName")
      Lude.<*> (x Lude..@? "ErrorCode")
      Lude.<*> (x Lude..@? "ErrorMessage")
