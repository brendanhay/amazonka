{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.NotifyAppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS SMS about whether application validation is successful.
module Network.AWS.SMS.NotifyAppValidationOutput
  ( -- * Creating a request
    NotifyAppValidationOutput (..),
    mkNotifyAppValidationOutput,

    -- ** Request lenses
    navoAppId,
    navoNotificationContext,

    -- * Destructuring the response
    NotifyAppValidationOutputResponse (..),
    mkNotifyAppValidationOutputResponse,

    -- ** Response lenses
    navorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkNotifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { -- | The ID of the application.
    appId :: Lude.Text,
    -- | The notification information.
    notificationContext :: Lude.Maybe NotificationContext
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyAppValidationOutput' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'notificationContext' - The notification information.
mkNotifyAppValidationOutput ::
  -- | 'appId'
  Lude.Text ->
  NotifyAppValidationOutput
mkNotifyAppValidationOutput pAppId_ =
  NotifyAppValidationOutput'
    { appId = pAppId_,
      notificationContext = Lude.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navoAppId :: Lens.Lens' NotifyAppValidationOutput Lude.Text
navoAppId = Lens.lens (appId :: NotifyAppValidationOutput -> Lude.Text) (\s a -> s {appId = a} :: NotifyAppValidationOutput)
{-# DEPRECATED navoAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The notification information.
--
-- /Note:/ Consider using 'notificationContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navoNotificationContext :: Lens.Lens' NotifyAppValidationOutput (Lude.Maybe NotificationContext)
navoNotificationContext = Lens.lens (notificationContext :: NotifyAppValidationOutput -> Lude.Maybe NotificationContext) (\s a -> s {notificationContext = a} :: NotifyAppValidationOutput)
{-# DEPRECATED navoNotificationContext "Use generic-lens or generic-optics with 'notificationContext' instead." #-}

instance Lude.AWSRequest NotifyAppValidationOutput where
  type
    Rs NotifyAppValidationOutput =
      NotifyAppValidationOutputResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          NotifyAppValidationOutputResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders NotifyAppValidationOutput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON NotifyAppValidationOutput where
  toJSON NotifyAppValidationOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("appId" Lude..= appId),
            ("notificationContext" Lude..=) Lude.<$> notificationContext
          ]
      )

instance Lude.ToPath NotifyAppValidationOutput where
  toPath = Lude.const "/"

instance Lude.ToQuery NotifyAppValidationOutput where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkNotifyAppValidationOutputResponse' smart constructor.
newtype NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyAppValidationOutputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkNotifyAppValidationOutputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  NotifyAppValidationOutputResponse
mkNotifyAppValidationOutputResponse pResponseStatus_ =
  NotifyAppValidationOutputResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navorsResponseStatus :: Lens.Lens' NotifyAppValidationOutputResponse Lude.Int
navorsResponseStatus = Lens.lens (responseStatus :: NotifyAppValidationOutputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: NotifyAppValidationOutputResponse)
{-# DEPRECATED navorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
