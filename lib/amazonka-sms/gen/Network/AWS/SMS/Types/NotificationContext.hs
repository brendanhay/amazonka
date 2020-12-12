{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.NotificationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.NotificationContext
  ( NotificationContext (..),

    -- * Smart constructor
    mkNotificationContext,

    -- * Lenses
    ncStatus,
    ncStatusMessage,
    ncValidationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains the status of validating an application.
--
-- /See:/ 'mkNotificationContext' smart constructor.
data NotificationContext = NotificationContext'
  { status ::
      Lude.Maybe ValidationStatus,
    statusMessage :: Lude.Maybe Lude.Text,
    validationId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationContext' with the minimum fields required to make a request.
--
-- * 'status' - The status of the validation.
-- * 'statusMessage' - The status message.
-- * 'validationId' - The ID of the validation.
mkNotificationContext ::
  NotificationContext
mkNotificationContext =
  NotificationContext'
    { status = Lude.Nothing,
      statusMessage = Lude.Nothing,
      validationId = Lude.Nothing
    }

-- | The status of the validation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncStatus :: Lens.Lens' NotificationContext (Lude.Maybe ValidationStatus)
ncStatus = Lens.lens (status :: NotificationContext -> Lude.Maybe ValidationStatus) (\s a -> s {status = a} :: NotificationContext)
{-# DEPRECATED ncStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncStatusMessage :: Lens.Lens' NotificationContext (Lude.Maybe Lude.Text)
ncStatusMessage = Lens.lens (statusMessage :: NotificationContext -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: NotificationContext)
{-# DEPRECATED ncStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncValidationId :: Lens.Lens' NotificationContext (Lude.Maybe Lude.Text)
ncValidationId = Lens.lens (validationId :: NotificationContext -> Lude.Maybe Lude.Text) (\s a -> s {validationId = a} :: NotificationContext)
{-# DEPRECATED ncValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

instance Lude.ToJSON NotificationContext where
  toJSON NotificationContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("statusMessage" Lude..=) Lude.<$> statusMessage,
            ("validationId" Lude..=) Lude.<$> validationId
          ]
      )
