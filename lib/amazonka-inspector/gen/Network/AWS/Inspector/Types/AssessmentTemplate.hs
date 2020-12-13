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
    atAssessmentTargetARN,
    atArn,
    atCreatedAt,
    atLastAssessmentRunARN,
    atUserAttributesForFindings,
    atRulesPackageARNs,
    atAssessmentRunCount,
    atName,
    atDurationInSeconds,
  )
where

import Network.AWS.Inspector.Types.Attribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector assessment template. This data type is used as the response element in the 'DescribeAssessmentTemplates' action.
--
-- /See:/ 'mkAssessmentTemplate' smart constructor.
data AssessmentTemplate = AssessmentTemplate'
  { -- | The ARN of the assessment target that corresponds to this assessment template.
    assessmentTargetARN :: Lude.Text,
    -- | The ARN of the assessment template.
    arn :: Lude.Text,
    -- | The time at which the assessment template is created.
    createdAt :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
    lastAssessmentRunARN :: Lude.Maybe Lude.Text,
    -- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
    userAttributesForFindings :: [Attribute],
    -- | The rules packages that are specified for this assessment template.
    rulesPackageARNs :: [Lude.Text],
    -- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
    assessmentRunCount :: Lude.Int,
    -- | The name of the assessment template.
    name :: Lude.Text,
    -- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
    durationInSeconds :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentTemplate' with the minimum fields required to make a request.
--
-- * 'assessmentTargetARN' - The ARN of the assessment target that corresponds to this assessment template.
-- * 'arn' - The ARN of the assessment template.
-- * 'createdAt' - The time at which the assessment template is created.
-- * 'lastAssessmentRunARN' - The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
-- * 'userAttributesForFindings' - The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
-- * 'rulesPackageARNs' - The rules packages that are specified for this assessment template.
-- * 'assessmentRunCount' - The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
-- * 'name' - The name of the assessment template.
-- * 'durationInSeconds' - The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
mkAssessmentTemplate ::
  -- | 'assessmentTargetARN'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Timestamp ->
  -- | 'assessmentRunCount'
  Lude.Int ->
  -- | 'name'
  Lude.Text ->
  -- | 'durationInSeconds'
  Lude.Natural ->
  AssessmentTemplate
mkAssessmentTemplate
  pAssessmentTargetARN_
  pArn_
  pCreatedAt_
  pAssessmentRunCount_
  pName_
  pDurationInSeconds_ =
    AssessmentTemplate'
      { assessmentTargetARN = pAssessmentTargetARN_,
        arn = pArn_,
        createdAt = pCreatedAt_,
        lastAssessmentRunARN = Lude.Nothing,
        userAttributesForFindings = Lude.mempty,
        rulesPackageARNs = Lude.mempty,
        assessmentRunCount = pAssessmentRunCount_,
        name = pName_,
        durationInSeconds = pDurationInSeconds_
      }

-- | The ARN of the assessment target that corresponds to this assessment template.
--
-- /Note:/ Consider using 'assessmentTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentTargetARN :: Lens.Lens' AssessmentTemplate Lude.Text
atAssessmentTargetARN = Lens.lens (assessmentTargetARN :: AssessmentTemplate -> Lude.Text) (\s a -> s {assessmentTargetARN = a} :: AssessmentTemplate)
{-# DEPRECATED atAssessmentTargetARN "Use generic-lens or generic-optics with 'assessmentTargetARN' instead." #-}

-- | The ARN of the assessment template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atArn :: Lens.Lens' AssessmentTemplate Lude.Text
atArn = Lens.lens (arn :: AssessmentTemplate -> Lude.Text) (\s a -> s {arn = a} :: AssessmentTemplate)
{-# DEPRECATED atArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time at which the assessment template is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atCreatedAt :: Lens.Lens' AssessmentTemplate Lude.Timestamp
atCreatedAt = Lens.lens (createdAt :: AssessmentTemplate -> Lude.Timestamp) (\s a -> s {createdAt = a} :: AssessmentTemplate)
{-# DEPRECATED atCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
--
-- /Note:/ Consider using 'lastAssessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atLastAssessmentRunARN :: Lens.Lens' AssessmentTemplate (Lude.Maybe Lude.Text)
atLastAssessmentRunARN = Lens.lens (lastAssessmentRunARN :: AssessmentTemplate -> Lude.Maybe Lude.Text) (\s a -> s {lastAssessmentRunARN = a} :: AssessmentTemplate)
{-# DEPRECATED atLastAssessmentRunARN "Use generic-lens or generic-optics with 'lastAssessmentRunARN' instead." #-}

-- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atUserAttributesForFindings :: Lens.Lens' AssessmentTemplate [Attribute]
atUserAttributesForFindings = Lens.lens (userAttributesForFindings :: AssessmentTemplate -> [Attribute]) (\s a -> s {userAttributesForFindings = a} :: AssessmentTemplate)
{-# DEPRECATED atUserAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead." #-}

-- | The rules packages that are specified for this assessment template.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atRulesPackageARNs :: Lens.Lens' AssessmentTemplate [Lude.Text]
atRulesPackageARNs = Lens.lens (rulesPackageARNs :: AssessmentTemplate -> [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: AssessmentTemplate)
{-# DEPRECATED atRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
--
-- /Note:/ Consider using 'assessmentRunCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentRunCount :: Lens.Lens' AssessmentTemplate Lude.Int
atAssessmentRunCount = Lens.lens (assessmentRunCount :: AssessmentTemplate -> Lude.Int) (\s a -> s {assessmentRunCount = a} :: AssessmentTemplate)
{-# DEPRECATED atAssessmentRunCount "Use generic-lens or generic-optics with 'assessmentRunCount' instead." #-}

-- | The name of the assessment template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AssessmentTemplate Lude.Text
atName = Lens.lens (name :: AssessmentTemplate -> Lude.Text) (\s a -> s {name = a} :: AssessmentTemplate)
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atDurationInSeconds :: Lens.Lens' AssessmentTemplate Lude.Natural
atDurationInSeconds = Lens.lens (durationInSeconds :: AssessmentTemplate -> Lude.Natural) (\s a -> s {durationInSeconds = a} :: AssessmentTemplate)
{-# DEPRECATED atDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

instance Lude.FromJSON AssessmentTemplate where
  parseJSON =
    Lude.withObject
      "AssessmentTemplate"
      ( \x ->
          AssessmentTemplate'
            Lude.<$> (x Lude..: "assessmentTargetArn")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..:? "lastAssessmentRunArn")
            Lude.<*> (x Lude..:? "userAttributesForFindings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "rulesPackageArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "assessmentRunCount")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "durationInSeconds")
      )
