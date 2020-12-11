{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified security configuration.
module Network.AWS.Glue.GetSecurityConfiguration
  ( -- * Creating a request
    GetSecurityConfiguration (..),
    mkGetSecurityConfiguration,

    -- ** Request lenses
    gscName,

    -- * Destructuring the response
    GetSecurityConfigurationResponse (..),
    mkGetSecurityConfigurationResponse,

    -- ** Response lenses
    gscrsSecurityConfiguration,
    gscrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSecurityConfiguration' smart constructor.
newtype GetSecurityConfiguration = GetSecurityConfiguration'
  { name ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the security configuration to retrieve.
mkGetSecurityConfiguration ::
  -- | 'name'
  Lude.Text ->
  GetSecurityConfiguration
mkGetSecurityConfiguration pName_ =
  GetSecurityConfiguration' {name = pName_}

-- | The name of the security configuration to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscName :: Lens.Lens' GetSecurityConfiguration Lude.Text
gscName = Lens.lens (name :: GetSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: GetSecurityConfiguration)
{-# DEPRECATED gscName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetSecurityConfiguration where
  type Rs GetSecurityConfiguration = GetSecurityConfigurationResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationResponse'
            Lude.<$> (x Lude..?> "SecurityConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSecurityConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetSecurityConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSecurityConfiguration where
  toJSON GetSecurityConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetSecurityConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSecurityConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSecurityConfigurationResponse' smart constructor.
data GetSecurityConfigurationResponse = GetSecurityConfigurationResponse'
  { securityConfiguration ::
      Lude.Maybe
        SecurityConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'securityConfiguration' - The requested security configuration.
mkGetSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSecurityConfigurationResponse
mkGetSecurityConfigurationResponse pResponseStatus_ =
  GetSecurityConfigurationResponse'
    { securityConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested security configuration.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsSecurityConfiguration :: Lens.Lens' GetSecurityConfigurationResponse (Lude.Maybe SecurityConfiguration)
gscrsSecurityConfiguration = Lens.lens (securityConfiguration :: GetSecurityConfigurationResponse -> Lude.Maybe SecurityConfiguration) (\s a -> s {securityConfiguration = a} :: GetSecurityConfigurationResponse)
{-# DEPRECATED gscrsSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsResponseStatus :: Lens.Lens' GetSecurityConfigurationResponse Lude.Int
gscrsResponseStatus = Lens.lens (responseStatus :: GetSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSecurityConfigurationResponse)
{-# DEPRECATED gscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
