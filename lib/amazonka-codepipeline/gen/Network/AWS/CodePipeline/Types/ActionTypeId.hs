{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionTypeId
  ( ActionTypeId (..)
  -- * Smart constructor
  , mkActionTypeId
  -- * Lenses
  , atiCategory
  , atiOwner
  , atiProvider
  , atiVersion
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionCategory as Types
import qualified Network.AWS.CodePipeline.Types.ActionOwner as Types
import qualified Network.AWS.CodePipeline.Types.ActionProvider as Types
import qualified Network.AWS.CodePipeline.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an action type.
--
-- /See:/ 'mkActionTypeId' smart constructor.
data ActionTypeId = ActionTypeId'
  { category :: Types.ActionCategory
    -- ^ A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values. 
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
  , owner :: Types.ActionOwner
    -- ^ The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
  , provider :: Types.ActionProvider
    -- ^ The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
  , version :: Types.Version
    -- ^ A string that describes the action version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionTypeId' value with any optional fields omitted.
mkActionTypeId
    :: Types.ActionCategory -- ^ 'category'
    -> Types.ActionOwner -- ^ 'owner'
    -> Types.ActionProvider -- ^ 'provider'
    -> Types.Version -- ^ 'version'
    -> ActionTypeId
mkActionTypeId category owner provider version
  = ActionTypeId'{category, owner, provider, version}

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
atiCategory :: Lens.Lens' ActionTypeId Types.ActionCategory
atiCategory = Lens.field @"category"
{-# INLINEABLE atiCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiOwner :: Lens.Lens' ActionTypeId Types.ActionOwner
atiOwner = Lens.field @"owner"
{-# INLINEABLE atiOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiProvider :: Lens.Lens' ActionTypeId Types.ActionProvider
atiProvider = Lens.field @"provider"
{-# INLINEABLE atiProvider #-}
{-# DEPRECATED provider "Use generic-lens or generic-optics with 'provider' instead"  #-}

-- | A string that describes the action version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiVersion :: Lens.Lens' ActionTypeId Types.Version
atiVersion = Lens.field @"version"
{-# INLINEABLE atiVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ActionTypeId where
        toJSON ActionTypeId{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("category" Core..= category),
                  Core.Just ("owner" Core..= owner),
                  Core.Just ("provider" Core..= provider),
                  Core.Just ("version" Core..= version)])

instance Core.FromJSON ActionTypeId where
        parseJSON
          = Core.withObject "ActionTypeId" Core.$
              \ x ->
                ActionTypeId' Core.<$>
                  (x Core..: "category") Core.<*> x Core..: "owner" Core.<*>
                    x Core..: "provider"
                    Core.<*> x Core..: "version"
