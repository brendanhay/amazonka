{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified development endpoint.
module Network.AWS.Glue.GetDevEndpoint
  ( -- * Creating a request
    GetDevEndpoint (..),
    mkGetDevEndpoint,

    -- ** Request lenses
    gdeEndpointName,

    -- * Destructuring the response
    GetDevEndpointResponse (..),
    mkGetDevEndpointResponse,

    -- ** Response lenses
    gdefrsDevEndpoint,
    gdefrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDevEndpoint' smart constructor.
newtype GetDevEndpoint = GetDevEndpoint'
  { -- | Name of the @DevEndpoint@ to retrieve information for.
    endpointName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointName' - Name of the @DevEndpoint@ to retrieve information for.
mkGetDevEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  GetDevEndpoint
mkGetDevEndpoint pEndpointName_ =
  GetDevEndpoint' {endpointName = pEndpointName_}

-- | Name of the @DevEndpoint@ to retrieve information for.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeEndpointName :: Lens.Lens' GetDevEndpoint Lude.Text
gdeEndpointName = Lens.lens (endpointName :: GetDevEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: GetDevEndpoint)
{-# DEPRECATED gdeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Lude.AWSRequest GetDevEndpoint where
  type Rs GetDevEndpoint = GetDevEndpointResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDevEndpointResponse'
            Lude.<$> (x Lude..?> "DevEndpoint") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetDevEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevEndpoint where
  toJSON GetDevEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointName" Lude..= endpointName)])

instance Lude.ToPath GetDevEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDevEndpointResponse' smart constructor.
data GetDevEndpointResponse = GetDevEndpointResponse'
  { -- | A @DevEndpoint@ definition.
    devEndpoint :: Lude.Maybe DevEndpoint,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevEndpointResponse' with the minimum fields required to make a request.
--
-- * 'devEndpoint' - A @DevEndpoint@ definition.
-- * 'responseStatus' - The response status code.
mkGetDevEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDevEndpointResponse
mkGetDevEndpointResponse pResponseStatus_ =
  GetDevEndpointResponse'
    { devEndpoint = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @DevEndpoint@ definition.
--
-- /Note:/ Consider using 'devEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdefrsDevEndpoint :: Lens.Lens' GetDevEndpointResponse (Lude.Maybe DevEndpoint)
gdefrsDevEndpoint = Lens.lens (devEndpoint :: GetDevEndpointResponse -> Lude.Maybe DevEndpoint) (\s a -> s {devEndpoint = a} :: GetDevEndpointResponse)
{-# DEPRECATED gdefrsDevEndpoint "Use generic-lens or generic-optics with 'devEndpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdefrsResponseStatus :: Lens.Lens' GetDevEndpointResponse Lude.Int
gdefrsResponseStatus = Lens.lens (responseStatus :: GetDevEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDevEndpointResponse)
{-# DEPRECATED gdefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
