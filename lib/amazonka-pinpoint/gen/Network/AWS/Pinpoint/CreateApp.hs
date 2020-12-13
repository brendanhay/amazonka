{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.Pinpoint.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caCreateApplicationRequest,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carsApplicationResponse,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateApp' smart constructor.
newtype CreateApp = CreateApp'
  { createApplicationRequest :: CreateApplicationRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- * 'createApplicationRequest' -
mkCreateApp ::
  -- | 'createApplicationRequest'
  CreateApplicationRequest ->
  CreateApp
mkCreateApp pCreateApplicationRequest_ =
  CreateApp' {createApplicationRequest = pCreateApplicationRequest_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createApplicationRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreateApplicationRequest :: Lens.Lens' CreateApp CreateApplicationRequest
caCreateApplicationRequest = Lens.lens (createApplicationRequest :: CreateApp -> CreateApplicationRequest) (\s a -> s {createApplicationRequest = a} :: CreateApp)
{-# DEPRECATED caCreateApplicationRequest "Use generic-lens or generic-optics with 'createApplicationRequest' instead." #-}

instance Lude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CreateApplicationRequest" Lude..= createApplicationRequest)
          ]
      )

instance Lude.ToPath CreateApp where
  toPath = Lude.const "/v1/apps"

instance Lude.ToQuery CreateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { applicationResponse :: ApplicationResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- * 'applicationResponse' -
-- * 'responseStatus' - The response status code.
mkCreateAppResponse ::
  -- | 'applicationResponse'
  ApplicationResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppResponse
mkCreateAppResponse pApplicationResponse_ pResponseStatus_ =
  CreateAppResponse'
    { applicationResponse = pApplicationResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsApplicationResponse :: Lens.Lens' CreateAppResponse ApplicationResponse
carsApplicationResponse = Lens.lens (applicationResponse :: CreateAppResponse -> ApplicationResponse) (\s a -> s {applicationResponse = a} :: CreateAppResponse)
{-# DEPRECATED carsApplicationResponse "Use generic-lens or generic-optics with 'applicationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAppResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
