{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified code signing configuration.
module Network.AWS.Lambda.GetCodeSigningConfig
  ( -- * Creating a request
    GetCodeSigningConfig (..),
    mkGetCodeSigningConfig,

    -- ** Request lenses
    gcscCodeSigningConfigARN,

    -- * Destructuring the response
    GetCodeSigningConfigResponse (..),
    mkGetCodeSigningConfigResponse,

    -- ** Response lenses
    gcscrsResponseStatus,
    gcscrsCodeSigningConfig,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCodeSigningConfig' smart constructor.
newtype GetCodeSigningConfig = GetCodeSigningConfig'
  { codeSigningConfigARN ::
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

-- | Creates a value of 'GetCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'codeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
mkGetCodeSigningConfig ::
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  GetCodeSigningConfig
mkGetCodeSigningConfig pCodeSigningConfigARN_ =
  GetCodeSigningConfig'
    { codeSigningConfigARN =
        pCodeSigningConfigARN_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscCodeSigningConfigARN :: Lens.Lens' GetCodeSigningConfig Lude.Text
gcscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: GetCodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: GetCodeSigningConfig)
{-# DEPRECATED gcscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

instance Lude.AWSRequest GetCodeSigningConfig where
  type Rs GetCodeSigningConfig = GetCodeSigningConfigResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCodeSigningConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeSigningConfig")
      )

instance Lude.ToHeaders GetCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCodeSigningConfig where
  toPath GetCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Lude.toBS codeSigningConfigARN
      ]

instance Lude.ToQuery GetCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCodeSigningConfigResponse' smart constructor.
data GetCodeSigningConfigResponse = GetCodeSigningConfigResponse'
  { responseStatus ::
      Lude.Int,
    codeSigningConfig ::
      CodeSigningConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'codeSigningConfig' - The code signing configuration
-- * 'responseStatus' - The response status code.
mkGetCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  GetCodeSigningConfigResponse
mkGetCodeSigningConfigResponse pResponseStatus_ pCodeSigningConfig_ =
  GetCodeSigningConfigResponse'
    { responseStatus = pResponseStatus_,
      codeSigningConfig = pCodeSigningConfig_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscrsResponseStatus :: Lens.Lens' GetCodeSigningConfigResponse Lude.Int
gcscrsResponseStatus = Lens.lens (responseStatus :: GetCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCodeSigningConfigResponse)
{-# DEPRECATED gcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The code signing configuration
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscrsCodeSigningConfig :: Lens.Lens' GetCodeSigningConfigResponse CodeSigningConfig
gcscrsCodeSigningConfig = Lens.lens (codeSigningConfig :: GetCodeSigningConfigResponse -> CodeSigningConfig) (\s a -> s {codeSigningConfig = a} :: GetCodeSigningConfigResponse)
{-# DEPRECATED gcscrsCodeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead." #-}
