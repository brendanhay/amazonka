{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCustomActionType (..),
    mkCreateCustomActionType,

    -- ** Request lenses
    ccatCategory,
    ccatProvider,
    ccatVersion,
    ccatInputArtifactDetails,
    ccatOutputArtifactDetails,
    ccatConfigurationProperties,
    ccatSettings,
    ccatTags,

    -- * Destructuring the response
    CreateCustomActionTypeResponse (..),
    mkCreateCustomActionTypeResponse,

    -- ** Response lenses
    ccatrrsActionType,
    ccatrrsTags,
    ccatrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateCustomActionType operation.
--
-- /See:/ 'mkCreateCustomActionType' smart constructor.
data CreateCustomActionType = CreateCustomActionType'
  { -- | The category of the custom action, such as a build action or a test action.
    category :: Types.ActionCategory,
    -- | The provider of the service used in the custom action, such as AWS CodeDeploy.
    provider :: Types.ActionProvider,
    -- | The version identifier of the custom action.
    version :: Types.Version,
    -- | The details of the input artifact for the action, such as its commit ID.
    inputArtifactDetails :: Types.ArtifactDetails,
    -- | The details of the output artifact of the action, such as its commit ID.
    outputArtifactDetails :: Types.ArtifactDetails,
    -- | The configuration properties for the custom action.
    configurationProperties :: Core.Maybe [Types.ActionConfigurationProperty],
    -- | URLs that provide users information about this custom action.
    settings :: Core.Maybe Types.ActionTypeSettings,
    -- | The tags for the custom action.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomActionType' value with any optional fields omitted.
mkCreateCustomActionType ::
  -- | 'category'
  Types.ActionCategory ->
  -- | 'provider'
  Types.ActionProvider ->
  -- | 'version'
  Types.Version ->
  -- | 'inputArtifactDetails'
  Types.ArtifactDetails ->
  -- | 'outputArtifactDetails'
  Types.ArtifactDetails ->
  CreateCustomActionType
mkCreateCustomActionType
  category
  provider
  version
  inputArtifactDetails
  outputArtifactDetails =
    CreateCustomActionType'
      { category,
        provider,
        version,
        inputArtifactDetails,
        outputArtifactDetails,
        configurationProperties = Core.Nothing,
        settings = Core.Nothing,
        tags = Core.Nothing
      }

-- | The category of the custom action, such as a build action or a test action.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatCategory :: Lens.Lens' CreateCustomActionType Types.ActionCategory
ccatCategory = Lens.field @"category"
{-# DEPRECATED ccatCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatProvider :: Lens.Lens' CreateCustomActionType Types.ActionProvider
ccatProvider = Lens.field @"provider"
{-# DEPRECATED ccatProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The version identifier of the custom action.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatVersion :: Lens.Lens' CreateCustomActionType Types.Version
ccatVersion = Lens.field @"version"
{-# DEPRECATED ccatVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The details of the input artifact for the action, such as its commit ID.
--
-- /Note:/ Consider using 'inputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatInputArtifactDetails :: Lens.Lens' CreateCustomActionType Types.ArtifactDetails
ccatInputArtifactDetails = Lens.field @"inputArtifactDetails"
{-# DEPRECATED ccatInputArtifactDetails "Use generic-lens or generic-optics with 'inputArtifactDetails' instead." #-}

-- | The details of the output artifact of the action, such as its commit ID.
--
-- /Note:/ Consider using 'outputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatOutputArtifactDetails :: Lens.Lens' CreateCustomActionType Types.ArtifactDetails
ccatOutputArtifactDetails = Lens.field @"outputArtifactDetails"
{-# DEPRECATED ccatOutputArtifactDetails "Use generic-lens or generic-optics with 'outputArtifactDetails' instead." #-}

-- | The configuration properties for the custom action.
--
-- /Note:/ Consider using 'configurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatConfigurationProperties :: Lens.Lens' CreateCustomActionType (Core.Maybe [Types.ActionConfigurationProperty])
ccatConfigurationProperties = Lens.field @"configurationProperties"
{-# DEPRECATED ccatConfigurationProperties "Use generic-lens or generic-optics with 'configurationProperties' instead." #-}

-- | URLs that provide users information about this custom action.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatSettings :: Lens.Lens' CreateCustomActionType (Core.Maybe Types.ActionTypeSettings)
ccatSettings = Lens.field @"settings"
{-# DEPRECATED ccatSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The tags for the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatTags :: Lens.Lens' CreateCustomActionType (Core.Maybe [Types.Tag])
ccatTags = Lens.field @"tags"
{-# DEPRECATED ccatTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateCustomActionType where
  toJSON CreateCustomActionType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("category" Core..= category),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version),
            Core.Just ("inputArtifactDetails" Core..= inputArtifactDetails),
            Core.Just ("outputArtifactDetails" Core..= outputArtifactDetails),
            ("configurationProperties" Core..=)
              Core.<$> configurationProperties,
            ("settings" Core..=) Core.<$> settings,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateCustomActionType where
  type Rs CreateCustomActionType = CreateCustomActionTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.CreateCustomActionType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomActionTypeResponse'
            Core.<$> (x Core..: "actionType")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreateCustomActionType@ operation.
--
-- /See:/ 'mkCreateCustomActionTypeResponse' smart constructor.
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
  { -- | Returns information about the details of an action type.
    actionType :: Types.ActionType,
    -- | Specifies the tags applied to the custom action.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomActionTypeResponse' value with any optional fields omitted.
mkCreateCustomActionTypeResponse ::
  -- | 'actionType'
  Types.ActionType ->
  -- | 'responseStatus'
  Core.Int ->
  CreateCustomActionTypeResponse
mkCreateCustomActionTypeResponse actionType responseStatus =
  CreateCustomActionTypeResponse'
    { actionType,
      tags = Core.Nothing,
      responseStatus
    }

-- | Returns information about the details of an action type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsActionType :: Lens.Lens' CreateCustomActionTypeResponse Types.ActionType
ccatrrsActionType = Lens.field @"actionType"
{-# DEPRECATED ccatrrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Specifies the tags applied to the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsTags :: Lens.Lens' CreateCustomActionTypeResponse (Core.Maybe [Types.Tag])
ccatrrsTags = Lens.field @"tags"
{-# DEPRECATED ccatrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrrsResponseStatus :: Lens.Lens' CreateCustomActionTypeResponse Core.Int
ccatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
