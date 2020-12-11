{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the code signing configuration. Changes to the code signing configuration take effect the next time a user tries to deploy a code package to the function.
module Network.AWS.Lambda.UpdateCodeSigningConfig
  ( -- * Creating a request
    UpdateCodeSigningConfig (..),
    mkUpdateCodeSigningConfig,

    -- ** Request lenses
    ucscAllowedPublishers,
    ucscCodeSigningPolicies,
    ucscDescription,
    ucscCodeSigningConfigARN,

    -- * Destructuring the response
    UpdateCodeSigningConfigResponse (..),
    mkUpdateCodeSigningConfigResponse,

    -- ** Response lenses
    ucscrsResponseStatus,
    ucscrsCodeSigningConfig,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCodeSigningConfig' smart constructor.
data UpdateCodeSigningConfig = UpdateCodeSigningConfig'
  { allowedPublishers ::
      Lude.Maybe AllowedPublishers,
    codeSigningPolicies ::
      Lude.Maybe CodeSigningPolicies,
    description :: Lude.Maybe Lude.Text,
    codeSigningConfigARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'allowedPublishers' - Signing profiles for this code signing configuration.
-- * 'codeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
-- * 'codeSigningPolicies' - The code signing policy.
-- * 'description' - Descriptive name for this code signing configuration.
mkUpdateCodeSigningConfig ::
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  UpdateCodeSigningConfig
mkUpdateCodeSigningConfig pCodeSigningConfigARN_ =
  UpdateCodeSigningConfig'
    { allowedPublishers = Lude.Nothing,
      codeSigningPolicies = Lude.Nothing,
      description = Lude.Nothing,
      codeSigningConfigARN = pCodeSigningConfigARN_
    }

-- | Signing profiles for this code signing configuration.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscAllowedPublishers :: Lens.Lens' UpdateCodeSigningConfig (Lude.Maybe AllowedPublishers)
ucscAllowedPublishers = Lens.lens (allowedPublishers :: UpdateCodeSigningConfig -> Lude.Maybe AllowedPublishers) (\s a -> s {allowedPublishers = a} :: UpdateCodeSigningConfig)
{-# DEPRECATED ucscAllowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead." #-}

-- | The code signing policy.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningPolicies :: Lens.Lens' UpdateCodeSigningConfig (Lude.Maybe CodeSigningPolicies)
ucscCodeSigningPolicies = Lens.lens (codeSigningPolicies :: UpdateCodeSigningConfig -> Lude.Maybe CodeSigningPolicies) (\s a -> s {codeSigningPolicies = a} :: UpdateCodeSigningConfig)
{-# DEPRECATED ucscCodeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead." #-}

-- | Descriptive name for this code signing configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscDescription :: Lens.Lens' UpdateCodeSigningConfig (Lude.Maybe Lude.Text)
ucscDescription = Lens.lens (description :: UpdateCodeSigningConfig -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateCodeSigningConfig)
{-# DEPRECATED ucscDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningConfigARN :: Lens.Lens' UpdateCodeSigningConfig Lude.Text
ucscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: UpdateCodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: UpdateCodeSigningConfig)
{-# DEPRECATED ucscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

instance Lude.AWSRequest UpdateCodeSigningConfig where
  type Rs UpdateCodeSigningConfig = UpdateCodeSigningConfigResponse
  request = Req.putJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCodeSigningConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeSigningConfig")
      )

instance Lude.ToHeaders UpdateCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateCodeSigningConfig where
  toJSON UpdateCodeSigningConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowedPublishers" Lude..=) Lude.<$> allowedPublishers,
            ("CodeSigningPolicies" Lude..=) Lude.<$> codeSigningPolicies,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateCodeSigningConfig where
  toPath UpdateCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Lude.toBS codeSigningConfigARN
      ]

instance Lude.ToQuery UpdateCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCodeSigningConfigResponse' smart constructor.
data UpdateCodeSigningConfigResponse = UpdateCodeSigningConfigResponse'
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

-- | Creates a value of 'UpdateCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'codeSigningConfig' - The code signing configuration
-- * 'responseStatus' - The response status code.
mkUpdateCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  UpdateCodeSigningConfigResponse
mkUpdateCodeSigningConfigResponse
  pResponseStatus_
  pCodeSigningConfig_ =
    UpdateCodeSigningConfigResponse'
      { responseStatus =
          pResponseStatus_,
        codeSigningConfig = pCodeSigningConfig_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrsResponseStatus :: Lens.Lens' UpdateCodeSigningConfigResponse Lude.Int
ucscrsResponseStatus = Lens.lens (responseStatus :: UpdateCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCodeSigningConfigResponse)
{-# DEPRECATED ucscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The code signing configuration
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrsCodeSigningConfig :: Lens.Lens' UpdateCodeSigningConfigResponse CodeSigningConfig
ucscrsCodeSigningConfig = Lens.lens (codeSigningConfig :: UpdateCodeSigningConfigResponse -> CodeSigningConfig) (\s a -> s {codeSigningConfig = a} :: UpdateCodeSigningConfigResponse)
{-# DEPRECATED ucscrsCodeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead." #-}
