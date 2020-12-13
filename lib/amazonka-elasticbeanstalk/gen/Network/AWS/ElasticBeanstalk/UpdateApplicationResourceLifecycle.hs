{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies lifecycle settings for an application.
module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
  ( -- * Creating a request
    UpdateApplicationResourceLifecycle (..),
    mkUpdateApplicationResourceLifecycle,

    -- ** Request lenses
    uarlApplicationName,
    uarlResourceLifecycleConfig,

    -- * Destructuring the response
    UpdateApplicationResourceLifecycleResponse (..),
    mkUpdateApplicationResourceLifecycleResponse,

    -- ** Response lenses
    uarlrsApplicationName,
    uarlrsResourceLifecycleConfig,
    uarlrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { -- | The name of the application.
    applicationName :: Lude.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: ApplicationResourceLifecycleConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationResourceLifecycle' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application.
-- * 'resourceLifecycleConfig' - The lifecycle configuration.
mkUpdateApplicationResourceLifecycle ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'resourceLifecycleConfig'
  ApplicationResourceLifecycleConfig ->
  UpdateApplicationResourceLifecycle
mkUpdateApplicationResourceLifecycle
  pApplicationName_
  pResourceLifecycleConfig_ =
    UpdateApplicationResourceLifecycle'
      { applicationName =
          pApplicationName_,
        resourceLifecycleConfig = pResourceLifecycleConfig_
      }

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycle Lude.Text
uarlApplicationName = Lens.lens (applicationName :: UpdateApplicationResourceLifecycle -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplicationResourceLifecycle)
{-# DEPRECATED uarlApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycle ApplicationResourceLifecycleConfig
uarlResourceLifecycleConfig = Lens.lens (resourceLifecycleConfig :: UpdateApplicationResourceLifecycle -> ApplicationResourceLifecycleConfig) (\s a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycle)
{-# DEPRECATED uarlResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

instance Lude.AWSRequest UpdateApplicationResourceLifecycle where
  type
    Rs UpdateApplicationResourceLifecycle =
      UpdateApplicationResourceLifecycleResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "UpdateApplicationResourceLifecycleResult"
      ( \s h x ->
          UpdateApplicationResourceLifecycleResponse'
            Lude.<$> (x Lude..@? "ApplicationName")
            Lude.<*> (x Lude..@? "ResourceLifecycleConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateApplicationResourceLifecycle where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateApplicationResourceLifecycle where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplicationResourceLifecycle where
  toQuery UpdateApplicationResourceLifecycle' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateApplicationResourceLifecycle" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ApplicationName" Lude.=: applicationName,
        "ResourceLifecycleConfig" Lude.=: resourceLifecycleConfig
      ]

-- | /See:/ 'mkUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { -- | The name of the application.
    applicationName :: Lude.Maybe Lude.Text,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: Lude.Maybe ApplicationResourceLifecycleConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationResourceLifecycleResponse' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application.
-- * 'resourceLifecycleConfig' - The lifecycle configuration.
-- * 'responseStatus' - The response status code.
mkUpdateApplicationResourceLifecycleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateApplicationResourceLifecycleResponse
mkUpdateApplicationResourceLifecycleResponse pResponseStatus_ =
  UpdateApplicationResourceLifecycleResponse'
    { applicationName =
        Lude.Nothing,
      resourceLifecycleConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrsApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Lude.Maybe Lude.Text)
uarlrsApplicationName = Lens.lens (applicationName :: UpdateApplicationResourceLifecycleResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplicationResourceLifecycleResponse)
{-# DEPRECATED uarlrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrsResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Lude.Maybe ApplicationResourceLifecycleConfig)
uarlrsResourceLifecycleConfig = Lens.lens (resourceLifecycleConfig :: UpdateApplicationResourceLifecycleResponse -> Lude.Maybe ApplicationResourceLifecycleConfig) (\s a -> s {resourceLifecycleConfig = a} :: UpdateApplicationResourceLifecycleResponse)
{-# DEPRECATED uarlrsResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrsResponseStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Lude.Int
uarlrsResponseStatus = Lens.lens (responseStatus :: UpdateApplicationResourceLifecycleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApplicationResourceLifecycleResponse)
{-# DEPRECATED uarlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
