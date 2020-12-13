{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a code signing configuration. A <https://docs.aws.amazon.com/lambda/latest/dg/configuration-trustedcode.html code signing configuration> defines a list of allowed signing profiles and defines the code-signing validation policy (action to be taken if deployment validation checks fail).
module Network.AWS.Lambda.CreateCodeSigningConfig
  ( -- * Creating a request
    CreateCodeSigningConfig (..),
    mkCreateCodeSigningConfig,

    -- ** Request lenses
    ccscAllowedPublishers,
    ccscCodeSigningPolicies,
    ccscDescription,

    -- * Destructuring the response
    CreateCodeSigningConfigResponse (..),
    mkCreateCodeSigningConfigResponse,

    -- ** Response lenses
    ccscrsCodeSigningConfig,
    ccscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCodeSigningConfig' smart constructor.
data CreateCodeSigningConfig = CreateCodeSigningConfig'
  { -- | Signing profiles for this code signing configuration.
    allowedPublishers :: AllowedPublishers,
    -- | The code signing policies define the actions to take if the validation checks fail.
    codeSigningPolicies :: Lude.Maybe CodeSigningPolicies,
    -- | Descriptive name for this code signing configuration.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'allowedPublishers' - Signing profiles for this code signing configuration.
-- * 'codeSigningPolicies' - The code signing policies define the actions to take if the validation checks fail.
-- * 'description' - Descriptive name for this code signing configuration.
mkCreateCodeSigningConfig ::
  -- | 'allowedPublishers'
  AllowedPublishers ->
  CreateCodeSigningConfig
mkCreateCodeSigningConfig pAllowedPublishers_ =
  CreateCodeSigningConfig'
    { allowedPublishers = pAllowedPublishers_,
      codeSigningPolicies = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Signing profiles for this code signing configuration.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscAllowedPublishers :: Lens.Lens' CreateCodeSigningConfig AllowedPublishers
ccscAllowedPublishers = Lens.lens (allowedPublishers :: CreateCodeSigningConfig -> AllowedPublishers) (\s a -> s {allowedPublishers = a} :: CreateCodeSigningConfig)
{-# DEPRECATED ccscAllowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead." #-}

-- | The code signing policies define the actions to take if the validation checks fail.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscCodeSigningPolicies :: Lens.Lens' CreateCodeSigningConfig (Lude.Maybe CodeSigningPolicies)
ccscCodeSigningPolicies = Lens.lens (codeSigningPolicies :: CreateCodeSigningConfig -> Lude.Maybe CodeSigningPolicies) (\s a -> s {codeSigningPolicies = a} :: CreateCodeSigningConfig)
{-# DEPRECATED ccscCodeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead." #-}

-- | Descriptive name for this code signing configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscDescription :: Lens.Lens' CreateCodeSigningConfig (Lude.Maybe Lude.Text)
ccscDescription = Lens.lens (description :: CreateCodeSigningConfig -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateCodeSigningConfig)
{-# DEPRECATED ccscDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateCodeSigningConfig where
  type Rs CreateCodeSigningConfig = CreateCodeSigningConfigResponse
  request = Req.postJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCodeSigningConfigResponse'
            Lude.<$> (x Lude..:> "CodeSigningConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateCodeSigningConfig where
  toJSON CreateCodeSigningConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AllowedPublishers" Lude..= allowedPublishers),
            ("CodeSigningPolicies" Lude..=) Lude.<$> codeSigningPolicies,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateCodeSigningConfig where
  toPath = Lude.const "/2020-04-22/code-signing-configs/"

instance Lude.ToQuery CreateCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCodeSigningConfigResponse' smart constructor.
data CreateCodeSigningConfigResponse = CreateCodeSigningConfigResponse'
  { -- | The code signing configuration.
    codeSigningConfig :: CodeSigningConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'codeSigningConfig' - The code signing configuration.
-- * 'responseStatus' - The response status code.
mkCreateCodeSigningConfigResponse ::
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateCodeSigningConfigResponse
mkCreateCodeSigningConfigResponse
  pCodeSigningConfig_
  pResponseStatus_ =
    CreateCodeSigningConfigResponse'
      { codeSigningConfig =
          pCodeSigningConfig_,
        responseStatus = pResponseStatus_
      }

-- | The code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscrsCodeSigningConfig :: Lens.Lens' CreateCodeSigningConfigResponse CodeSigningConfig
ccscrsCodeSigningConfig = Lens.lens (codeSigningConfig :: CreateCodeSigningConfigResponse -> CodeSigningConfig) (\s a -> s {codeSigningConfig = a} :: CreateCodeSigningConfigResponse)
{-# DEPRECATED ccscrsCodeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscrsResponseStatus :: Lens.Lens' CreateCodeSigningConfigResponse Lude.Int
ccscrsResponseStatus = Lens.lens (responseStatus :: CreateCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCodeSigningConfigResponse)
{-# DEPRECATED ccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
