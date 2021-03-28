{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.CreateCustomActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom action that can be used in all pipelines associated with the AWS account. Only used for custom actions.
module Network.AWS.CodePipeline.CreateCustomActionType
    (
    -- * Creating a request
      CreateCustomActionType (..)
    , mkCreateCustomActionType
    -- ** Request lenses
    , ccatCategory
    , ccatProvider
    , ccatVersion
    , ccatInputArtifactDetails
    , ccatOutputArtifactDetails
    , ccatConfigurationProperties
    , ccatSettings
    , ccatTags

    -- * Destructuring the response
    , CreateCustomActionTypeResponse (..)
    , mkCreateCustomActionTypeResponse
    -- ** Response lenses
    , ccatrrsActionType
    , ccatrrsTags
    , ccatrrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateCustomActionType operation.
--
-- /See:/ 'mkCreateCustomActionType' smart constructor.
data CreateCustomActionType = CreateCustomActionType'
  { category :: Types.ActionCategory
    -- ^ The category of the custom action, such as a build action or a test action.
  , provider :: Types.ActionProvider
    -- ^ The provider of the service used in the custom action, such as AWS CodeDeploy.
  , version :: Types.Version
    -- ^ The version identifier of the custom action.
  , inputArtifactDetails :: Types.ArtifactDetails
    -- ^ The details of the input artifact for the action, such as its commit ID.
  , outputArtifactDetails :: Types.ArtifactDetails
    -- ^ The details of the output artifact of the action, such as its commit ID.
  , configurationProperties :: Core.Maybe [Types.ActionConfigurationProperty]
    -- ^ The configuration properties for the custom action.
  , settings :: Core.Maybe Types.ActionTypeSettings
    -- ^ URLs that provide users information about this custom action.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the custom action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomActionType' value with any optional fields omitted.
mkCreateCustomActionType
    :: Types.ActionCategory -- ^ 'category'
    -> Types.ActionProvider -- ^ 'provider'
    -> Types.Version -- ^ 'version'
    -> Types.ArtifactDetails -- ^ 'inputArtifactDetails'
    -> Types.ArtifactDetails -- ^ 'outputArtifactDetails'
    -> CreateCustomActionType
mkCreateCustomActionType category provider version
  inputArtifactDetails outputArtifactDetails
  = CreateCustomActionType'{category, provider, version,
                            inputArtifactDetails, outputArtifactDetails,
                            configurationProperties = Core.Nothing, settings = Core.Nothing,
                            tags = Core.Nothing}

-- | The category of the custom action, such as a build action or a test action.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatCategory :: Lens.Lens' CreateCustomActionType Types.ActionCategory
ccatCategory = Lens.field @"category"
{-# INLINEABLE ccatCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatProvider :: Lens.Lens' CreateCustomActionType Types.ActionProvider
ccatProvider = Lens.field @"provider"
{-# INLINEABLE ccatProvider #-}
{-# DEPRECATED provider "Use generic-lens or generic-optics with 'provider' instead"  #-}

-- | The version identifier of the custom action.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatVersion :: Lens.Lens' CreateCustomActionType Types.Version
ccatVersion = Lens.field @"version"
{-# INLINEABLE ccatVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The details of the input artifact for the action, such as its commit ID.
--
-- /Note:/ Consider using 'inputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatInputArtifactDetails :: Lens.Lens' CreateCustomActionType Types.ArtifactDetails
ccatInputArtifactDetails = Lens.field @"inputArtifactDetails"
{-# INLINEABLE ccatInputArtifactDetails #-}
{-# DEPRECATED inputArtifactDetails "Use generic-lens or generic-optics with 'inputArtifactDetails' instead"  #-}

-- | The details of the output artifact of the action, such as its commit ID.
--
-- /Note:/ Consider using 'outputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatOutputArtifactDetails :: Lens.Lens' CreateCustomActionType Types.ArtifactDetails
ccatOutputArtifactDetails = Lens.field @"outputArtifactDetails"
{-# INLINEABLE ccatOutputArtifactDetails #-}
{-# DEPRECATED outputArtifactDetails "Use generic-lens or generic-optics with 'outputArtifactDetails' instead"  #-}

-- | The configuration properties for the custom action.
--
-- /Note:/ Consider using 'configurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatConfigurationProperties :: Lens.Lens' CreateCustomActionType (Core.Maybe [Types.ActionConfigurationProperty])
ccatConfigurationProperties = Lens.field @"configurationProperties"
{-# INLINEABLE ccatConfigurationProperties #-}
{-# DEPRECATED configurationProperties "Use generic-lens or generic-optics with 'configurationProperties' instead"  #-}

-- | URLs that provide users information about this custom action.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatSettings :: Lens.Lens' CreateCustomActionType (Core.Maybe Types.ActionTypeSettings)
ccatSettings = Lens.field @"settings"
{-# INLINEABLE ccatSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | The tags for the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatTags :: Lens.Lens' CreateCustomActionType (Core.Maybe [Types.Tag])
ccatTags = Lens.field @"tags"
{-# INLINEABLE ccatTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCustomActionType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCustomActionType where
        toHeaders CreateCustomActionType{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.CreateCustomActionType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCustomActionType where
        toJSON CreateCustomActionType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("category" Core..= category),
                  Core.Just ("provider" Core..= provider),
                  Core.Just ("version" Core..= version),
                  Core.Just ("inputArtifactDetails" Core..= inputArtifactDetails),
                  Core.Just ("outputArtifactDetails" Core..= outputArtifactDetails),
                  ("configurationProperties" Core..=) Core.<$>
                    configurationProperties,
                  ("settings" Core..=) Core.<$> settings,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCustomActionType where
        type Rs CreateCustomActionType = CreateCustomActionTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCustomActionTypeResponse' Core.<$>
                   (x Core..: "actionType") Core.<*> x Core..:? "tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateCustomActionType@ operation.
--
-- /See:/ 'mkCreateCustomActionTypeResponse' smart constructor.
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
  { actionType :: Types.ActionType
    -- ^ Returns information about the details of an action type.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the custom action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomActionTypeResponse' value with any optional fields omitted.
mkCreateCustomActionTypeResponse
    :: Types.ActionType -- ^ 'actionType'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateCustomActionTypeResponse
mkCreateCustomActionTypeResponse actionType responseStatus
  = CreateCustomActionTypeResponse'{actionType, tags = Core.Nothing,
                                    responseStatus}

-- | Returns information about the details of an action type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsActionType :: Lens.Lens' CreateCustomActionTypeResponse Types.ActionType
ccatrrsActionType = Lens.field @"actionType"
{-# INLINEABLE ccatrrsActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | Specifies the tags applied to the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsTags :: Lens.Lens' CreateCustomActionTypeResponse (Core.Maybe [Types.Tag])
ccatrrsTags = Lens.field @"tags"
{-# INLINEABLE ccatrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsResponseStatus :: Lens.Lens' CreateCustomActionTypeResponse Core.Int
ccatrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccatrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
