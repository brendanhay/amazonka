{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTemplate
  ( AssessmentTemplate (..),

    -- * Smart constructor
    mkAssessmentTemplate,

    -- * Lenses
    atArn,
    atName,
    atAssessmentTargetArn,
    atDurationInSeconds,
    atRulesPackageArns,
    atUserAttributesForFindings,
    atAssessmentRunCount,
    atCreatedAt,
    atLastAssessmentRunArn,
  )
where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector assessment template. This data type is used as the response element in the 'DescribeAssessmentTemplates' action.
--
-- /See:/ 'mkAssessmentTemplate' smart constructor.
data AssessmentTemplate = AssessmentTemplate'
  { -- | The ARN of the assessment template.
    arn :: Types.Arn,
    -- | The name of the assessment template.
    name :: Types.Name,
    -- | The ARN of the assessment target that corresponds to this assessment template.
    assessmentTargetArn :: Types.Arn,
    -- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
    durationInSeconds :: Core.Natural,
    -- | The rules packages that are specified for this assessment template.
    rulesPackageArns :: [Types.Arn],
    -- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
    userAttributesForFindings :: [Types.Attribute],
    -- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
    assessmentRunCount :: Core.Int,
    -- | The time at which the assessment template is created.
    createdAt :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
    lastAssessmentRunArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssessmentTemplate' value with any optional fields omitted.
mkAssessmentTemplate ::
  -- | 'arn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  -- | 'assessmentTargetArn'
  Types.Arn ->
  -- | 'durationInSeconds'
  Core.Natural ->
  -- | 'assessmentRunCount'
  Core.Int ->
  -- | 'createdAt'
  Core.NominalDiffTime ->
  AssessmentTemplate
mkAssessmentTemplate
  arn
  name
  assessmentTargetArn
  durationInSeconds
  assessmentRunCount
  createdAt =
    AssessmentTemplate'
      { arn,
        name,
        assessmentTargetArn,
        durationInSeconds,
        rulesPackageArns = Core.mempty,
        userAttributesForFindings = Core.mempty,
        assessmentRunCount,
        createdAt,
        lastAssessmentRunArn = Core.Nothing
      }

-- | The ARN of the assessment template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atArn :: Lens.Lens' AssessmentTemplate Types.Arn
atArn = Lens.field @"arn"
{-# DEPRECATED atArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the assessment template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AssessmentTemplate Types.Name
atName = Lens.field @"name"
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the assessment target that corresponds to this assessment template.
--
-- /Note:/ Consider using 'assessmentTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentTargetArn :: Lens.Lens' AssessmentTemplate Types.Arn
atAssessmentTargetArn = Lens.field @"assessmentTargetArn"
{-# DEPRECATED atAssessmentTargetArn "Use generic-lens or generic-optics with 'assessmentTargetArn' instead." #-}

-- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atDurationInSeconds :: Lens.Lens' AssessmentTemplate Core.Natural
atDurationInSeconds = Lens.field @"durationInSeconds"
{-# DEPRECATED atDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

-- | The rules packages that are specified for this assessment template.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atRulesPackageArns :: Lens.Lens' AssessmentTemplate [Types.Arn]
atRulesPackageArns = Lens.field @"rulesPackageArns"
{-# DEPRECATED atRulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead." #-}

-- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atUserAttributesForFindings :: Lens.Lens' AssessmentTemplate [Types.Attribute]
atUserAttributesForFindings = Lens.field @"userAttributesForFindings"
{-# DEPRECATED atUserAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead." #-}

-- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
--
-- /Note:/ Consider using 'assessmentRunCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentRunCount :: Lens.Lens' AssessmentTemplate Core.Int
atAssessmentRunCount = Lens.field @"assessmentRunCount"
{-# DEPRECATED atAssessmentRunCount "Use generic-lens or generic-optics with 'assessmentRunCount' instead." #-}

-- | The time at which the assessment template is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atCreatedAt :: Lens.Lens' AssessmentTemplate Core.NominalDiffTime
atCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED atCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
--
-- /Note:/ Consider using 'lastAssessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atLastAssessmentRunArn :: Lens.Lens' AssessmentTemplate (Core.Maybe Types.Arn)
atLastAssessmentRunArn = Lens.field @"lastAssessmentRunArn"
{-# DEPRECATED atLastAssessmentRunArn "Use generic-lens or generic-optics with 'lastAssessmentRunArn' instead." #-}

instance Core.FromJSON AssessmentTemplate where
  parseJSON =
    Core.withObject "AssessmentTemplate" Core.$
      \x ->
        AssessmentTemplate'
          Core.<$> (x Core..: "arn")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "assessmentTargetArn")
          Core.<*> (x Core..: "durationInSeconds")
          Core.<*> (x Core..:? "rulesPackageArns" Core..!= Core.mempty)
          Core.<*> (x Core..:? "userAttributesForFindings" Core..!= Core.mempty)
          Core.<*> (x Core..: "assessmentRunCount")
          Core.<*> (x Core..: "createdAt")
          Core.<*> (x Core..:? "lastAssessmentRunArn")
