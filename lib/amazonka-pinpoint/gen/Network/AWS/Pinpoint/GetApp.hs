{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an application.
module Network.AWS.Pinpoint.GetApp
  ( -- * Creating a request
    GetApp (..),
    mkGetApp,

    -- ** Request lenses
    gaApplicationId,

    -- * Destructuring the response
    GetAppResponse (..),
    mkGetAppResponse,

    -- ** Response lenses
    garsApplicationResponse,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetApp' smart constructor.
newtype GetApp = GetApp'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApp' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetApp ::
  -- | 'applicationId'
  Lude.Text ->
  GetApp
mkGetApp pApplicationId_ = GetApp' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationId :: Lens.Lens' GetApp Lude.Text
gaApplicationId = Lens.lens (applicationId :: GetApp -> Lude.Text) (\s a -> s {applicationId = a} :: GetApp)
{-# DEPRECATED gaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetApp where
  type Rs GetApp = GetAppResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApp where
  toPath GetApp' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId]

instance Lude.ToQuery GetApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { applicationResponse :: ApplicationResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppResponse' with the minimum fields required to make a request.
--
-- * 'applicationResponse' -
-- * 'responseStatus' - The response status code.
mkGetAppResponse ::
  -- | 'applicationResponse'
  ApplicationResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAppResponse
mkGetAppResponse pApplicationResponse_ pResponseStatus_ =
  GetAppResponse'
    { applicationResponse = pApplicationResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsApplicationResponse :: Lens.Lens' GetAppResponse ApplicationResponse
garsApplicationResponse = Lens.lens (applicationResponse :: GetAppResponse -> ApplicationResponse) (\s a -> s {applicationResponse = a} :: GetAppResponse)
{-# DEPRECATED garsApplicationResponse "Use generic-lens or generic-optics with 'applicationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAppResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
