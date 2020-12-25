{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ucscCodeSigningConfigArn,
    ucscAllowedPublishers,
    ucscCodeSigningPolicies,
    ucscDescription,

    -- * Destructuring the response
    UpdateCodeSigningConfigResponse (..),
    mkUpdateCodeSigningConfigResponse,

    -- ** Response lenses
    ucscrrsCodeSigningConfig,
    ucscrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCodeSigningConfig' smart constructor.
data UpdateCodeSigningConfig = UpdateCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Types.CodeSigningConfigArn,
    -- | Signing profiles for this code signing configuration.
    allowedPublishers :: Core.Maybe Types.AllowedPublishers,
    -- | The code signing policy.
    codeSigningPolicies :: Core.Maybe Types.CodeSigningPolicies,
    -- | Descriptive name for this code signing configuration.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeSigningConfig' value with any optional fields omitted.
mkUpdateCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Types.CodeSigningConfigArn ->
  UpdateCodeSigningConfig
mkUpdateCodeSigningConfig codeSigningConfigArn =
  UpdateCodeSigningConfig'
    { codeSigningConfigArn,
      allowedPublishers = Core.Nothing,
      codeSigningPolicies = Core.Nothing,
      description = Core.Nothing
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningConfigArn :: Lens.Lens' UpdateCodeSigningConfig Types.CodeSigningConfigArn
ucscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# DEPRECATED ucscCodeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead." #-}

-- | Signing profiles for this code signing configuration.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscAllowedPublishers :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.AllowedPublishers)
ucscAllowedPublishers = Lens.field @"allowedPublishers"
{-# DEPRECATED ucscAllowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead." #-}

-- | The code signing policy.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningPolicies :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.CodeSigningPolicies)
ucscCodeSigningPolicies = Lens.field @"codeSigningPolicies"
{-# DEPRECATED ucscCodeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead." #-}

-- | Descriptive name for this code signing configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscDescription :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.Description)
ucscDescription = Lens.field @"description"
{-# DEPRECATED ucscDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON UpdateCodeSigningConfig where
  toJSON UpdateCodeSigningConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowedPublishers" Core..=) Core.<$> allowedPublishers,
            ("CodeSigningPolicies" Core..=) Core.<$> codeSigningPolicies,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest UpdateCodeSigningConfig where
  type Rs UpdateCodeSigningConfig = UpdateCodeSigningConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2020-04-22/code-signing-configs/"
                Core.<> (Core.toText codeSigningConfigArn)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCodeSigningConfigResponse'
            Core.<$> (x Core..: "CodeSigningConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCodeSigningConfigResponse' smart constructor.
data UpdateCodeSigningConfigResponse = UpdateCodeSigningConfigResponse'
  { -- | The code signing configuration
    codeSigningConfig :: Types.CodeSigningConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeSigningConfigResponse' value with any optional fields omitted.
mkUpdateCodeSigningConfigResponse ::
  -- | 'codeSigningConfig'
  Types.CodeSigningConfig ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateCodeSigningConfigResponse
mkUpdateCodeSigningConfigResponse codeSigningConfig responseStatus =
  UpdateCodeSigningConfigResponse'
    { codeSigningConfig,
      responseStatus
    }

-- | The code signing configuration
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrrsCodeSigningConfig :: Lens.Lens' UpdateCodeSigningConfigResponse Types.CodeSigningConfig
ucscrrsCodeSigningConfig = Lens.field @"codeSigningConfig"
{-# DEPRECATED ucscrrsCodeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrrsResponseStatus :: Lens.Lens' UpdateCodeSigningConfigResponse Core.Int
ucscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
