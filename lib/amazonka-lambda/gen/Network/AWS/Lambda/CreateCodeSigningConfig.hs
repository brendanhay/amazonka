{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCodeSigningConfig (..)
    , mkCreateCodeSigningConfig
    -- ** Request lenses
    , ccscAllowedPublishers
    , ccscCodeSigningPolicies
    , ccscDescription

    -- * Destructuring the response
    , CreateCodeSigningConfigResponse (..)
    , mkCreateCodeSigningConfigResponse
    -- ** Response lenses
    , ccscrrsCodeSigningConfig
    , ccscrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCodeSigningConfig' smart constructor.
data CreateCodeSigningConfig = CreateCodeSigningConfig'
  { allowedPublishers :: Types.AllowedPublishers
    -- ^ Signing profiles for this code signing configuration.
  , codeSigningPolicies :: Core.Maybe Types.CodeSigningPolicies
    -- ^ The code signing policies define the actions to take if the validation checks fail. 
  , description :: Core.Maybe Types.Description
    -- ^ Descriptive name for this code signing configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCodeSigningConfig' value with any optional fields omitted.
mkCreateCodeSigningConfig
    :: Types.AllowedPublishers -- ^ 'allowedPublishers'
    -> CreateCodeSigningConfig
mkCreateCodeSigningConfig allowedPublishers
  = CreateCodeSigningConfig'{allowedPublishers,
                             codeSigningPolicies = Core.Nothing, description = Core.Nothing}

-- | Signing profiles for this code signing configuration.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscAllowedPublishers :: Lens.Lens' CreateCodeSigningConfig Types.AllowedPublishers
ccscAllowedPublishers = Lens.field @"allowedPublishers"
{-# INLINEABLE ccscAllowedPublishers #-}
{-# DEPRECATED allowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead"  #-}

-- | The code signing policies define the actions to take if the validation checks fail. 
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscCodeSigningPolicies :: Lens.Lens' CreateCodeSigningConfig (Core.Maybe Types.CodeSigningPolicies)
ccscCodeSigningPolicies = Lens.field @"codeSigningPolicies"
{-# INLINEABLE ccscCodeSigningPolicies #-}
{-# DEPRECATED codeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead"  #-}

-- | Descriptive name for this code signing configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscDescription :: Lens.Lens' CreateCodeSigningConfig (Core.Maybe Types.Description)
ccscDescription = Lens.field @"description"
{-# INLINEABLE ccscDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateCodeSigningConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCodeSigningConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateCodeSigningConfig where
        toJSON CreateCodeSigningConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AllowedPublishers" Core..= allowedPublishers),
                  ("CodeSigningPolicies" Core..=) Core.<$> codeSigningPolicies,
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest CreateCodeSigningConfig where
        type Rs CreateCodeSigningConfig = CreateCodeSigningConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-04-22/code-signing-configs/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCodeSigningConfigResponse' Core.<$>
                   (x Core..: "CodeSigningConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCodeSigningConfigResponse' smart constructor.
data CreateCodeSigningConfigResponse = CreateCodeSigningConfigResponse'
  { codeSigningConfig :: Types.CodeSigningConfig
    -- ^ The code signing configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCodeSigningConfigResponse' value with any optional fields omitted.
mkCreateCodeSigningConfigResponse
    :: Types.CodeSigningConfig -- ^ 'codeSigningConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateCodeSigningConfigResponse
mkCreateCodeSigningConfigResponse codeSigningConfig responseStatus
  = CreateCodeSigningConfigResponse'{codeSigningConfig,
                                     responseStatus}

-- | The code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscrrsCodeSigningConfig :: Lens.Lens' CreateCodeSigningConfigResponse Types.CodeSigningConfig
ccscrrsCodeSigningConfig = Lens.field @"codeSigningConfig"
{-# INLINEABLE ccscrrsCodeSigningConfig #-}
{-# DEPRECATED codeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscrrsResponseStatus :: Lens.Lens' CreateCodeSigningConfigResponse Core.Int
ccscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
