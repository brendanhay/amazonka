{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeId
  ( ActionTypeId (..),

    -- * Smart constructor
    mkActionTypeId,

    -- * Lenses
    atiCategory,
    atiOwner,
    atiVersion,
    atiProvider,
  )
where

import Network.AWS.CodePipeline.Types.ActionCategory
import Network.AWS.CodePipeline.Types.ActionOwner
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an action type.
--
-- /See:/ 'mkActionTypeId' smart constructor.
data ActionTypeId = ActionTypeId'
  { -- | A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values.
    --
    --
    --     * Source
    --
    --
    --     * Build
    --
    --
    --     * Test
    --
    --
    --     * Deploy
    --
    --
    --     * Invoke
    --
    --
    --     * Approval
    category :: ActionCategory,
    -- | The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
    owner :: ActionOwner,
    -- | A string that describes the action version.
    version :: Lude.Text,
    -- | The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
    provider :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionTypeId' with the minimum fields required to make a request.
--
-- * 'category' - A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values.
--
--
--     * Source
--
--
--     * Build
--
--
--     * Test
--
--
--     * Deploy
--
--
--     * Invoke
--
--
--     * Approval
--
--
-- * 'owner' - The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
-- * 'version' - A string that describes the action version.
-- * 'provider' - The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
mkActionTypeId ::
  -- | 'category'
  ActionCategory ->
  -- | 'owner'
  ActionOwner ->
  -- | 'version'
  Lude.Text ->
  -- | 'provider'
  Lude.Text ->
  ActionTypeId
mkActionTypeId pCategory_ pOwner_ pVersion_ pProvider_ =
  ActionTypeId'
    { category = pCategory_,
      owner = pOwner_,
      version = pVersion_,
      provider = pProvider_
    }

-- | A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values.
--
--
--     * Source
--
--
--     * Build
--
--
--     * Test
--
--
--     * Deploy
--
--
--     * Invoke
--
--
--     * Approval
--
--
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiCategory :: Lens.Lens' ActionTypeId ActionCategory
atiCategory = Lens.lens (category :: ActionTypeId -> ActionCategory) (\s a -> s {category = a} :: ActionTypeId)
{-# DEPRECATED atiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiOwner :: Lens.Lens' ActionTypeId ActionOwner
atiOwner = Lens.lens (owner :: ActionTypeId -> ActionOwner) (\s a -> s {owner = a} :: ActionTypeId)
{-# DEPRECATED atiOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | A string that describes the action version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiVersion :: Lens.Lens' ActionTypeId Lude.Text
atiVersion = Lens.lens (version :: ActionTypeId -> Lude.Text) (\s a -> s {version = a} :: ActionTypeId)
{-# DEPRECATED atiVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiProvider :: Lens.Lens' ActionTypeId Lude.Text
atiProvider = Lens.lens (provider :: ActionTypeId -> Lude.Text) (\s a -> s {provider = a} :: ActionTypeId)
{-# DEPRECATED atiProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

instance Lude.FromJSON ActionTypeId where
  parseJSON =
    Lude.withObject
      "ActionTypeId"
      ( \x ->
          ActionTypeId'
            Lude.<$> (x Lude..: "category")
            Lude.<*> (x Lude..: "owner")
            Lude.<*> (x Lude..: "version")
            Lude.<*> (x Lude..: "provider")
      )

instance Lude.ToJSON ActionTypeId where
  toJSON ActionTypeId' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("category" Lude..= category),
            Lude.Just ("owner" Lude..= owner),
            Lude.Just ("version" Lude..= version),
            Lude.Just ("provider" Lude..= provider)
          ]
      )
