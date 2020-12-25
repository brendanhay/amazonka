{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorServiceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorServiceAttributes
  ( InspectorServiceAttributes (..),

    -- * Smart constructor
    mkInspectorServiceAttributes,

    -- * Lenses
    isaSchemaVersion,
    isaAssessmentRunArn,
    isaRulesPackageArn,
  )
where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'Finding' data type.
--
-- /See:/ 'mkInspectorServiceAttributes' smart constructor.
data InspectorServiceAttributes = InspectorServiceAttributes'
  { -- | The schema version of this data type.
    schemaVersion :: Core.Natural,
    -- | The ARN of the assessment run during which the finding is generated.
    assessmentRunArn :: Core.Maybe Types.Arn,
    -- | The ARN of the rules package that is used to generate the finding.
    rulesPackageArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InspectorServiceAttributes' value with any optional fields omitted.
mkInspectorServiceAttributes ::
  -- | 'schemaVersion'
  Core.Natural ->
  InspectorServiceAttributes
mkInspectorServiceAttributes schemaVersion =
  InspectorServiceAttributes'
    { schemaVersion,
      assessmentRunArn = Core.Nothing,
      rulesPackageArn = Core.Nothing
    }

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaSchemaVersion :: Lens.Lens' InspectorServiceAttributes Core.Natural
isaSchemaVersion = Lens.field @"schemaVersion"
{-# DEPRECATED isaSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The ARN of the assessment run during which the finding is generated.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaAssessmentRunArn :: Lens.Lens' InspectorServiceAttributes (Core.Maybe Types.Arn)
isaAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# DEPRECATED isaAssessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead." #-}

-- | The ARN of the rules package that is used to generate the finding.
--
-- /Note:/ Consider using 'rulesPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isaRulesPackageArn :: Lens.Lens' InspectorServiceAttributes (Core.Maybe Types.Arn)
isaRulesPackageArn = Lens.field @"rulesPackageArn"
{-# DEPRECATED isaRulesPackageArn "Use generic-lens or generic-optics with 'rulesPackageArn' instead." #-}

instance Core.FromJSON InspectorServiceAttributes where
  parseJSON =
    Core.withObject "InspectorServiceAttributes" Core.$
      \x ->
        InspectorServiceAttributes'
          Core.<$> (x Core..: "schemaVersion")
          Core.<*> (x Core..:? "assessmentRunArn")
          Core.<*> (x Core..:? "rulesPackageArn")
