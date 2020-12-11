{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccatSettings,
    ccatConfigurationProperties,
    ccatTags,
    ccatCategory,
    ccatProvider,
    ccatVersion,
    ccatInputArtifactDetails,
    ccatOutputArtifactDetails,

    -- * Destructuring the response
    CreateCustomActionTypeResponse (..),
    mkCreateCustomActionTypeResponse,

    -- ** Response lenses
    ccatrsTags,
    ccatrsResponseStatus,
    ccatrsActionType,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a CreateCustomActionType operation.
--
-- /See:/ 'mkCreateCustomActionType' smart constructor.
data CreateCustomActionType = CreateCustomActionType'
  { settings ::
      Lude.Maybe ActionTypeSettings,
    configurationProperties ::
      Lude.Maybe [ActionConfigurationProperty],
    tags :: Lude.Maybe [Tag],
    category :: ActionCategory,
    provider :: Lude.Text,
    version :: Lude.Text,
    inputArtifactDetails :: ArtifactDetails,
    outputArtifactDetails :: ArtifactDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomActionType' with the minimum fields required to make a request.
--
-- * 'category' - The category of the custom action, such as a build action or a test action.
-- * 'configurationProperties' - The configuration properties for the custom action.
-- * 'inputArtifactDetails' - The details of the input artifact for the action, such as its commit ID.
-- * 'outputArtifactDetails' - The details of the output artifact of the action, such as its commit ID.
-- * 'provider' - The provider of the service used in the custom action, such as AWS CodeDeploy.
-- * 'settings' - URLs that provide users information about this custom action.
-- * 'tags' - The tags for the custom action.
-- * 'version' - The version identifier of the custom action.
mkCreateCustomActionType ::
  -- | 'category'
  ActionCategory ->
  -- | 'provider'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  -- | 'inputArtifactDetails'
  ArtifactDetails ->
  -- | 'outputArtifactDetails'
  ArtifactDetails ->
  CreateCustomActionType
mkCreateCustomActionType
  pCategory_
  pProvider_
  pVersion_
  pInputArtifactDetails_
  pOutputArtifactDetails_ =
    CreateCustomActionType'
      { settings = Lude.Nothing,
        configurationProperties = Lude.Nothing,
        tags = Lude.Nothing,
        category = pCategory_,
        provider = pProvider_,
        version = pVersion_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | URLs that provide users information about this custom action.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatSettings :: Lens.Lens' CreateCustomActionType (Lude.Maybe ActionTypeSettings)
ccatSettings = Lens.lens (settings :: CreateCustomActionType -> Lude.Maybe ActionTypeSettings) (\s a -> s {settings = a} :: CreateCustomActionType)
{-# DEPRECATED ccatSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The configuration properties for the custom action.
--
-- /Note:/ Consider using 'configurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatConfigurationProperties :: Lens.Lens' CreateCustomActionType (Lude.Maybe [ActionConfigurationProperty])
ccatConfigurationProperties = Lens.lens (configurationProperties :: CreateCustomActionType -> Lude.Maybe [ActionConfigurationProperty]) (\s a -> s {configurationProperties = a} :: CreateCustomActionType)
{-# DEPRECATED ccatConfigurationProperties "Use generic-lens or generic-optics with 'configurationProperties' instead." #-}

-- | The tags for the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatTags :: Lens.Lens' CreateCustomActionType (Lude.Maybe [Tag])
ccatTags = Lens.lens (tags :: CreateCustomActionType -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCustomActionType)
{-# DEPRECATED ccatTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The category of the custom action, such as a build action or a test action.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatCategory :: Lens.Lens' CreateCustomActionType ActionCategory
ccatCategory = Lens.lens (category :: CreateCustomActionType -> ActionCategory) (\s a -> s {category = a} :: CreateCustomActionType)
{-# DEPRECATED ccatCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatProvider :: Lens.Lens' CreateCustomActionType Lude.Text
ccatProvider = Lens.lens (provider :: CreateCustomActionType -> Lude.Text) (\s a -> s {provider = a} :: CreateCustomActionType)
{-# DEPRECATED ccatProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The version identifier of the custom action.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatVersion :: Lens.Lens' CreateCustomActionType Lude.Text
ccatVersion = Lens.lens (version :: CreateCustomActionType -> Lude.Text) (\s a -> s {version = a} :: CreateCustomActionType)
{-# DEPRECATED ccatVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The details of the input artifact for the action, such as its commit ID.
--
-- /Note:/ Consider using 'inputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatInputArtifactDetails :: Lens.Lens' CreateCustomActionType ArtifactDetails
ccatInputArtifactDetails = Lens.lens (inputArtifactDetails :: CreateCustomActionType -> ArtifactDetails) (\s a -> s {inputArtifactDetails = a} :: CreateCustomActionType)
{-# DEPRECATED ccatInputArtifactDetails "Use generic-lens or generic-optics with 'inputArtifactDetails' instead." #-}

-- | The details of the output artifact of the action, such as its commit ID.
--
-- /Note:/ Consider using 'outputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatOutputArtifactDetails :: Lens.Lens' CreateCustomActionType ArtifactDetails
ccatOutputArtifactDetails = Lens.lens (outputArtifactDetails :: CreateCustomActionType -> ArtifactDetails) (\s a -> s {outputArtifactDetails = a} :: CreateCustomActionType)
{-# DEPRECATED ccatOutputArtifactDetails "Use generic-lens or generic-optics with 'outputArtifactDetails' instead." #-}

instance Lude.AWSRequest CreateCustomActionType where
  type Rs CreateCustomActionType = CreateCustomActionTypeResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCustomActionTypeResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "actionType")
      )

instance Lude.ToHeaders CreateCustomActionType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.CreateCustomActionType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCustomActionType where
  toJSON CreateCustomActionType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("settings" Lude..=) Lude.<$> settings,
            ("configurationProperties" Lude..=)
              Lude.<$> configurationProperties,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("category" Lude..= category),
            Lude.Just ("provider" Lude..= provider),
            Lude.Just ("version" Lude..= version),
            Lude.Just ("inputArtifactDetails" Lude..= inputArtifactDetails),
            Lude.Just ("outputArtifactDetails" Lude..= outputArtifactDetails)
          ]
      )

instance Lude.ToPath CreateCustomActionType where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCustomActionType where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateCustomActionType@ operation.
--
-- /See:/ 'mkCreateCustomActionTypeResponse' smart constructor.
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
  { tags ::
      Lude.Maybe [Tag],
    responseStatus :: Lude.Int,
    actionType :: ActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomActionTypeResponse' with the minimum fields required to make a request.
--
-- * 'actionType' - Returns information about the details of an action type.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Specifies the tags applied to the custom action.
mkCreateCustomActionTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'actionType'
  ActionType ->
  CreateCustomActionTypeResponse
mkCreateCustomActionTypeResponse pResponseStatus_ pActionType_ =
  CreateCustomActionTypeResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_,
      actionType = pActionType_
    }

-- | Specifies the tags applied to the custom action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrsTags :: Lens.Lens' CreateCustomActionTypeResponse (Lude.Maybe [Tag])
ccatrsTags = Lens.lens (tags :: CreateCustomActionTypeResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCustomActionTypeResponse)
{-# DEPRECATED ccatrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrsResponseStatus :: Lens.Lens' CreateCustomActionTypeResponse Lude.Int
ccatrsResponseStatus = Lens.lens (responseStatus :: CreateCustomActionTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCustomActionTypeResponse)
{-# DEPRECATED ccatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Returns information about the details of an action type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccatrsActionType :: Lens.Lens' CreateCustomActionTypeResponse ActionType
ccatrsActionType = Lens.lens (actionType :: CreateCustomActionTypeResponse -> ActionType) (\s a -> s {actionType = a} :: CreateCustomActionTypeResponse)
{-# DEPRECATED ccatrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}
