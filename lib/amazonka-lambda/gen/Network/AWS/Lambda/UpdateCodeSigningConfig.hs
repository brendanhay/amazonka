{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateCodeSigningConfig (..)
    , mkUpdateCodeSigningConfig
    -- ** Request lenses
    , ucscCodeSigningConfigArn
    , ucscAllowedPublishers
    , ucscCodeSigningPolicies
    , ucscDescription

    -- * Destructuring the response
    , UpdateCodeSigningConfigResponse (..)
    , mkUpdateCodeSigningConfigResponse
    -- ** Response lenses
    , ucscrrsCodeSigningConfig
    , ucscrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCodeSigningConfig' smart constructor.
data UpdateCodeSigningConfig = UpdateCodeSigningConfig'
  { codeSigningConfigArn :: Types.CodeSigningConfigArn
    -- ^ The The Amazon Resource Name (ARN) of the code signing configuration.
  , allowedPublishers :: Core.Maybe Types.AllowedPublishers
    -- ^ Signing profiles for this code signing configuration.
  , codeSigningPolicies :: Core.Maybe Types.CodeSigningPolicies
    -- ^ The code signing policy.
  , description :: Core.Maybe Types.Description
    -- ^ Descriptive name for this code signing configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeSigningConfig' value with any optional fields omitted.
mkUpdateCodeSigningConfig
    :: Types.CodeSigningConfigArn -- ^ 'codeSigningConfigArn'
    -> UpdateCodeSigningConfig
mkUpdateCodeSigningConfig codeSigningConfigArn
  = UpdateCodeSigningConfig'{codeSigningConfigArn,
                             allowedPublishers = Core.Nothing,
                             codeSigningPolicies = Core.Nothing, description = Core.Nothing}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningConfigArn :: Lens.Lens' UpdateCodeSigningConfig Types.CodeSigningConfigArn
ucscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# INLINEABLE ucscCodeSigningConfigArn #-}
{-# DEPRECATED codeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead"  #-}

-- | Signing profiles for this code signing configuration.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscAllowedPublishers :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.AllowedPublishers)
ucscAllowedPublishers = Lens.field @"allowedPublishers"
{-# INLINEABLE ucscAllowedPublishers #-}
{-# DEPRECATED allowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead"  #-}

-- | The code signing policy.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscCodeSigningPolicies :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.CodeSigningPolicies)
ucscCodeSigningPolicies = Lens.field @"codeSigningPolicies"
{-# INLINEABLE ucscCodeSigningPolicies #-}
{-# DEPRECATED codeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead"  #-}

-- | Descriptive name for this code signing configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscDescription :: Lens.Lens' UpdateCodeSigningConfig (Core.Maybe Types.Description)
ucscDescription = Lens.field @"description"
{-# INLINEABLE ucscDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateCodeSigningConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCodeSigningConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateCodeSigningConfig where
        toJSON UpdateCodeSigningConfig{..}
          = Core.object
              (Core.catMaybes
                 [("AllowedPublishers" Core..=) Core.<$> allowedPublishers,
                  ("CodeSigningPolicies" Core..=) Core.<$> codeSigningPolicies,
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest UpdateCodeSigningConfig where
        type Rs UpdateCodeSigningConfig = UpdateCodeSigningConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-04-22/code-signing-configs/" Core.<>
                             Core.toText codeSigningConfigArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateCodeSigningConfigResponse' Core.<$>
                   (x Core..: "CodeSigningConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCodeSigningConfigResponse' smart constructor.
data UpdateCodeSigningConfigResponse = UpdateCodeSigningConfigResponse'
  { codeSigningConfig :: Types.CodeSigningConfig
    -- ^ The code signing configuration
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeSigningConfigResponse' value with any optional fields omitted.
mkUpdateCodeSigningConfigResponse
    :: Types.CodeSigningConfig -- ^ 'codeSigningConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateCodeSigningConfigResponse
mkUpdateCodeSigningConfigResponse codeSigningConfig responseStatus
  = UpdateCodeSigningConfigResponse'{codeSigningConfig,
                                     responseStatus}

-- | The code signing configuration
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrrsCodeSigningConfig :: Lens.Lens' UpdateCodeSigningConfigResponse Types.CodeSigningConfig
ucscrrsCodeSigningConfig = Lens.field @"codeSigningConfig"
{-# INLINEABLE ucscrrsCodeSigningConfig #-}
{-# DEPRECATED codeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucscrrsResponseStatus :: Lens.Lens' UpdateCodeSigningConfigResponse Core.Int
ucscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
