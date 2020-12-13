{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.TerminateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the stack for the specified application.
module Network.AWS.SMS.TerminateApp
  ( -- * Creating a request
    TerminateApp (..),
    mkTerminateApp,

    -- ** Request lenses
    taAppId,

    -- * Destructuring the response
    TerminateAppResponse (..),
    mkTerminateAppResponse,

    -- ** Response lenses
    tarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkTerminateApp' smart constructor.
newtype TerminateApp = TerminateApp'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateApp' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkTerminateApp ::
  TerminateApp
mkTerminateApp = TerminateApp' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAppId :: Lens.Lens' TerminateApp (Lude.Maybe Lude.Text)
taAppId = Lens.lens (appId :: TerminateApp -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: TerminateApp)
{-# DEPRECATED taAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest TerminateApp where
  type Rs TerminateApp = TerminateAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TerminateAppResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.TerminateApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateApp where
  toJSON TerminateApp' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath TerminateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateAppResponse' smart constructor.
newtype TerminateAppResponse = TerminateAppResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateAppResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTerminateAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateAppResponse
mkTerminateAppResponse pResponseStatus_ =
  TerminateAppResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResponseStatus :: Lens.Lens' TerminateAppResponse Lude.Int
tarsResponseStatus = Lens.lens (responseStatus :: TerminateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateAppResponse)
{-# DEPRECATED tarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
