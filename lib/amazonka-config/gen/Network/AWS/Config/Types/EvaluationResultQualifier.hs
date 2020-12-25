{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultQualifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultQualifier
  ( EvaluationResultQualifier (..),

    -- * Smart constructor
    mkEvaluationResultQualifier,

    -- * Lenses
    erqConfigRuleName,
    erqResourceId,
    erqResourceType,
  )
where

import qualified Network.AWS.Config.Types.BaseResourceId as Types
import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.
--
-- /See:/ 'mkEvaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
  { -- | The name of the AWS Config rule that was used in the evaluation.
    configRuleName :: Core.Maybe Types.ConfigRuleName,
    -- | The ID of the evaluated AWS resource.
    resourceId :: Core.Maybe Types.BaseResourceId,
    -- | The type of AWS resource that was evaluated.
    resourceType :: Core.Maybe Types.StringWithCharLimit256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluationResultQualifier' value with any optional fields omitted.
mkEvaluationResultQualifier ::
  EvaluationResultQualifier
mkEvaluationResultQualifier =
  EvaluationResultQualifier'
    { configRuleName = Core.Nothing,
      resourceId = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The name of the AWS Config rule that was used in the evaluation.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqConfigRuleName :: Lens.Lens' EvaluationResultQualifier (Core.Maybe Types.ConfigRuleName)
erqConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED erqConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The ID of the evaluated AWS resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqResourceId :: Lens.Lens' EvaluationResultQualifier (Core.Maybe Types.BaseResourceId)
erqResourceId = Lens.field @"resourceId"
{-# DEPRECATED erqResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erqResourceType :: Lens.Lens' EvaluationResultQualifier (Core.Maybe Types.StringWithCharLimit256)
erqResourceType = Lens.field @"resourceType"
{-# DEPRECATED erqResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON EvaluationResultQualifier where
  parseJSON =
    Core.withObject "EvaluationResultQualifier" Core.$
      \x ->
        EvaluationResultQualifier'
          Core.<$> (x Core..:? "ConfigRuleName")
          Core.<*> (x Core..:? "ResourceId")
          Core.<*> (x Core..:? "ResourceType")
