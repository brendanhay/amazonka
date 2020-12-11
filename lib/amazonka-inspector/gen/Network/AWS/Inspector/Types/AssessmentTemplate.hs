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
    atLastAssessmentRunARN,
    atArn,
    atName,
    atAssessmentTargetARN,
    atDurationInSeconds,
    atRulesPackageARNs,
    atUserAttributesForFindings,
    atAssessmentRunCount,
    atCreatedAt,
  )
where

import Network.AWS.Inspector.Types.Attribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector assessment template. This data type is used as the response element in the 'DescribeAssessmentTemplates' action.
--
-- /See:/ 'mkAssessmentTemplate' smart constructor.
data AssessmentTemplate = AssessmentTemplate'
  { lastAssessmentRunARN ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Text,
    name :: Lude.Text,
    assessmentTargetARN :: Lude.Text,
    durationInSeconds :: Lude.Natural,
    rulesPackageARNs :: [Lude.Text],
    userAttributesForFindings :: [Attribute],
    assessmentRunCount :: Lude.Int,
    createdAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentTemplate' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the assessment template.
-- * 'assessmentRunCount' - The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
-- * 'assessmentTargetARN' - The ARN of the assessment target that corresponds to this assessment template.
-- * 'createdAt' - The time at which the assessment template is created.
-- * 'durationInSeconds' - The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
-- * 'lastAssessmentRunARN' - The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
-- * 'name' - The name of the assessment template.
-- * 'rulesPackageARNs' - The rules packages that are specified for this assessment template.
-- * 'userAttributesForFindings' - The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
mkAssessmentTemplate ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'assessmentTargetARN'
  Lude.Text ->
  -- | 'durationInSeconds'
  Lude.Natural ->
  -- | 'assessmentRunCount'
  Lude.Int ->
  -- | 'createdAt'
  Lude.Timestamp ->
  AssessmentTemplate
mkAssessmentTemplate
  pArn_
  pName_
  pAssessmentTargetARN_
  pDurationInSeconds_
  pAssessmentRunCount_
  pCreatedAt_ =
    AssessmentTemplate'
      { lastAssessmentRunARN = Lude.Nothing,
        arn = pArn_,
        name = pName_,
        assessmentTargetARN = pAssessmentTargetARN_,
        durationInSeconds = pDurationInSeconds_,
        rulesPackageARNs = Lude.mempty,
        userAttributesForFindings = Lude.mempty,
        assessmentRunCount = pAssessmentRunCount_,
        createdAt = pCreatedAt_
      }

-- | The Amazon Resource Name (ARN) of the most recent assessment run associated with this assessment template. This value exists only when the value of assessmentRunCount is greaterpa than zero.
--
-- /Note:/ Consider using 'lastAssessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atLastAssessmentRunARN :: Lens.Lens' AssessmentTemplate (Lude.Maybe Lude.Text)
atLastAssessmentRunARN = Lens.lens (lastAssessmentRunARN :: AssessmentTemplate -> Lude.Maybe Lude.Text) (\s a -> s {lastAssessmentRunARN = a} :: AssessmentTemplate)
{-# DEPRECATED atLastAssessmentRunARN "Use generic-lens or generic-optics with 'lastAssessmentRunARN' instead." #-}

-- | The ARN of the assessment template.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atArn :: Lens.Lens' AssessmentTemplate Lude.Text
atArn = Lens.lens (arn :: AssessmentTemplate -> Lude.Text) (\s a -> s {arn = a} :: AssessmentTemplate)
{-# DEPRECATED atArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the assessment template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AssessmentTemplate Lude.Text
atName = Lens.lens (name :: AssessmentTemplate -> Lude.Text) (\s a -> s {name = a} :: AssessmentTemplate)
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the assessment target that corresponds to this assessment template.
--
-- /Note:/ Consider using 'assessmentTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentTargetARN :: Lens.Lens' AssessmentTemplate Lude.Text
atAssessmentTargetARN = Lens.lens (assessmentTargetARN :: AssessmentTemplate -> Lude.Text) (\s a -> s {assessmentTargetARN = a} :: AssessmentTemplate)
{-# DEPRECATED atAssessmentTargetARN "Use generic-lens or generic-optics with 'assessmentTargetARN' instead." #-}

-- | The duration in seconds specified for this assessment template. The default value is 3600 seconds (one hour). The maximum value is 86400 seconds (one day).
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atDurationInSeconds :: Lens.Lens' AssessmentTemplate Lude.Natural
atDurationInSeconds = Lens.lens (durationInSeconds :: AssessmentTemplate -> Lude.Natural) (\s a -> s {durationInSeconds = a} :: AssessmentTemplate)
{-# DEPRECATED atDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

-- | The rules packages that are specified for this assessment template.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atRulesPackageARNs :: Lens.Lens' AssessmentTemplate [Lude.Text]
atRulesPackageARNs = Lens.lens (rulesPackageARNs :: AssessmentTemplate -> [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: AssessmentTemplate)
{-# DEPRECATED atRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | The user-defined attributes that are assigned to every generated finding from the assessment run that uses this assessment template.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atUserAttributesForFindings :: Lens.Lens' AssessmentTemplate [Attribute]
atUserAttributesForFindings = Lens.lens (userAttributesForFindings :: AssessmentTemplate -> [Attribute]) (\s a -> s {userAttributesForFindings = a} :: AssessmentTemplate)
{-# DEPRECATED atUserAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead." #-}

-- | The number of existing assessment runs associated with this assessment template. This value can be zero or a positive integer.
--
-- /Note:/ Consider using 'assessmentRunCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAssessmentRunCount :: Lens.Lens' AssessmentTemplate Lude.Int
atAssessmentRunCount = Lens.lens (assessmentRunCount :: AssessmentTemplate -> Lude.Int) (\s a -> s {assessmentRunCount = a} :: AssessmentTemplate)
{-# DEPRECATED atAssessmentRunCount "Use generic-lens or generic-optics with 'assessmentRunCount' instead." #-}

-- | The time at which the assessment template is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atCreatedAt :: Lens.Lens' AssessmentTemplate Lude.Timestamp
atCreatedAt = Lens.lens (createdAt :: AssessmentTemplate -> Lude.Timestamp) (\s a -> s {createdAt = a} :: AssessmentTemplate)
{-# DEPRECATED atCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

instance Lude.FromJSON AssessmentTemplate where
  parseJSON =
    Lude.withObject
      "AssessmentTemplate"
      ( \x ->
          AssessmentTemplate'
            Lude.<$> (x Lude..:? "lastAssessmentRunArn")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "assessmentTargetArn")
            Lude.<*> (x Lude..: "durationInSeconds")
            Lude.<*> (x Lude..:? "rulesPackageArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "userAttributesForFindings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "assessmentRunCount")
            Lude.<*> (x Lude..: "createdAt")
      )
