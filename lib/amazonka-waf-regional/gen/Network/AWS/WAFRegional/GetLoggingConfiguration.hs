{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'LoggingConfiguration' for the specified web ACL.
module Network.AWS.WAFRegional.GetLoggingConfiguration
  ( -- * Creating a request
    GetLoggingConfiguration (..),
    mkGetLoggingConfiguration,

    -- ** Request lenses
    glcResourceARN,

    -- * Destructuring the response
    GetLoggingConfigurationResponse (..),
    mkGetLoggingConfigurationResponse,

    -- ** Response lenses
    glcrsLoggingConfiguration,
    glcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetLoggingConfiguration' smart constructor.
newtype GetLoggingConfiguration = GetLoggingConfiguration'
  { resourceARN ::
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

-- | Creates a value of 'GetLoggingConfiguration' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
mkGetLoggingConfiguration ::
  -- | 'resourceARN'
  Lude.Text ->
  GetLoggingConfiguration
mkGetLoggingConfiguration pResourceARN_ =
  GetLoggingConfiguration' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcResourceARN :: Lens.Lens' GetLoggingConfiguration Lude.Text
glcResourceARN = Lens.lens (resourceARN :: GetLoggingConfiguration -> Lude.Text) (\s a -> s {resourceARN = a} :: GetLoggingConfiguration)
{-# DEPRECATED glcResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetLoggingConfiguration where
  type Rs GetLoggingConfiguration = GetLoggingConfigurationResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoggingConfigurationResponse'
            Lude.<$> (x Lude..?> "LoggingConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoggingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.GetLoggingConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLoggingConfiguration where
  toJSON GetLoggingConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetLoggingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLoggingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { loggingConfiguration ::
      Lude.Maybe
        LoggingConfiguration,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'loggingConfiguration' - The 'LoggingConfiguration' for the specified web ACL.
-- * 'responseStatus' - The response status code.
mkGetLoggingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoggingConfigurationResponse
mkGetLoggingConfigurationResponse pResponseStatus_ =
  GetLoggingConfigurationResponse'
    { loggingConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'LoggingConfiguration' for the specified web ACL.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrsLoggingConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Lude.Maybe LoggingConfiguration)
glcrsLoggingConfiguration = Lens.lens (loggingConfiguration :: GetLoggingConfigurationResponse -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: GetLoggingConfigurationResponse)
{-# DEPRECATED glcrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrsResponseStatus :: Lens.Lens' GetLoggingConfigurationResponse Lude.Int
glcrsResponseStatus = Lens.lens (responseStatus :: GetLoggingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoggingConfigurationResponse)
{-# DEPRECATED glcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
