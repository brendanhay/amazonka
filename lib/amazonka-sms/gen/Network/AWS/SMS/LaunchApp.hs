{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.LaunchApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified application as a stack in AWS CloudFormation.
module Network.AWS.SMS.LaunchApp
  ( -- * Creating a request
    LaunchApp (..),
    mkLaunchApp,

    -- ** Request lenses
    laAppId,

    -- * Destructuring the response
    LaunchAppResponse (..),
    mkLaunchAppResponse,

    -- ** Response lenses
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkLaunchApp' smart constructor.
newtype LaunchApp = LaunchApp' {appId :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchApp' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkLaunchApp ::
  LaunchApp
mkLaunchApp = LaunchApp' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAppId :: Lens.Lens' LaunchApp (Lude.Maybe Lude.Text)
laAppId = Lens.lens (appId :: LaunchApp -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: LaunchApp)
{-# DEPRECATED laAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest LaunchApp where
  type Rs LaunchApp = LaunchAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          LaunchAppResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders LaunchApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.LaunchApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON LaunchApp where
  toJSON LaunchApp' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath LaunchApp where
  toPath = Lude.const "/"

instance Lude.ToQuery LaunchApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkLaunchAppResponse' smart constructor.
newtype LaunchAppResponse = LaunchAppResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchAppResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkLaunchAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  LaunchAppResponse
mkLaunchAppResponse pResponseStatus_ =
  LaunchAppResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' LaunchAppResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: LaunchAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: LaunchAppResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
