{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequirement
  ( QualificationRequirement (..),

    -- * Smart constructor
    mkQualificationRequirement,

    -- * Lenses
    qrLocaleValues,
    qrActionsGuarded,
    qrRequiredToPreview,
    qrIntegerValues,
    qrQualificationTypeId,
    qrComparator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.Comparator
import Network.AWS.MechanicalTurk.Types.HITAccessActions
import Network.AWS.MechanicalTurk.Types.Locale
import qualified Network.AWS.Prelude as Lude

-- | The QualificationRequirement data structure describes a Qualification that a Worker must have before the Worker is allowed to accept a HIT. A requirement may optionally state that a Worker must have the Qualification in order to preview the HIT, or see the HIT in search results.
--
-- /See:/ 'mkQualificationRequirement' smart constructor.
data QualificationRequirement = QualificationRequirement'
  { localeValues ::
      Lude.Maybe [Locale],
    actionsGuarded ::
      Lude.Maybe HITAccessActions,
    requiredToPreview :: Lude.Maybe Lude.Bool,
    integerValues :: Lude.Maybe [Lude.Int],
    qualificationTypeId :: Lude.Text,
    comparator :: Comparator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QualificationRequirement' with the minimum fields required to make a request.
--
-- * 'actionsGuarded' - Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
-- * 'comparator' - The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
-- * 'integerValues' - The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
-- * 'localeValues' - The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
-- * 'qualificationTypeId' - The ID of the Qualification type for the requirement.
-- * 'requiredToPreview' - DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
mkQualificationRequirement ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  -- | 'comparator'
  Comparator ->
  QualificationRequirement
mkQualificationRequirement pQualificationTypeId_ pComparator_ =
  QualificationRequirement'
    { localeValues = Lude.Nothing,
      actionsGuarded = Lude.Nothing,
      requiredToPreview = Lude.Nothing,
      integerValues = Lude.Nothing,
      qualificationTypeId = pQualificationTypeId_,
      comparator = pComparator_
    }

-- | The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
--
-- /Note:/ Consider using 'localeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrLocaleValues :: Lens.Lens' QualificationRequirement (Lude.Maybe [Locale])
qrLocaleValues = Lens.lens (localeValues :: QualificationRequirement -> Lude.Maybe [Locale]) (\s a -> s {localeValues = a} :: QualificationRequirement)
{-# DEPRECATED qrLocaleValues "Use generic-lens or generic-optics with 'localeValues' instead." #-}

-- | Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
--
-- /Note:/ Consider using 'actionsGuarded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrActionsGuarded :: Lens.Lens' QualificationRequirement (Lude.Maybe HITAccessActions)
qrActionsGuarded = Lens.lens (actionsGuarded :: QualificationRequirement -> Lude.Maybe HITAccessActions) (\s a -> s {actionsGuarded = a} :: QualificationRequirement)
{-# DEPRECATED qrActionsGuarded "Use generic-lens or generic-optics with 'actionsGuarded' instead." #-}

-- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
--
-- /Note:/ Consider using 'requiredToPreview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrRequiredToPreview :: Lens.Lens' QualificationRequirement (Lude.Maybe Lude.Bool)
qrRequiredToPreview = Lens.lens (requiredToPreview :: QualificationRequirement -> Lude.Maybe Lude.Bool) (\s a -> s {requiredToPreview = a} :: QualificationRequirement)
{-# DEPRECATED qrRequiredToPreview "Use generic-lens or generic-optics with 'requiredToPreview' instead." #-}

-- | The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
--
-- /Note:/ Consider using 'integerValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrIntegerValues :: Lens.Lens' QualificationRequirement (Lude.Maybe [Lude.Int])
qrIntegerValues = Lens.lens (integerValues :: QualificationRequirement -> Lude.Maybe [Lude.Int]) (\s a -> s {integerValues = a} :: QualificationRequirement)
{-# DEPRECATED qrIntegerValues "Use generic-lens or generic-optics with 'integerValues' instead." #-}

-- | The ID of the Qualification type for the requirement.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrQualificationTypeId :: Lens.Lens' QualificationRequirement Lude.Text
qrQualificationTypeId = Lens.lens (qualificationTypeId :: QualificationRequirement -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: QualificationRequirement)
{-# DEPRECATED qrQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
--
-- /Note:/ Consider using 'comparator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrComparator :: Lens.Lens' QualificationRequirement Comparator
qrComparator = Lens.lens (comparator :: QualificationRequirement -> Comparator) (\s a -> s {comparator = a} :: QualificationRequirement)
{-# DEPRECATED qrComparator "Use generic-lens or generic-optics with 'comparator' instead." #-}

instance Lude.FromJSON QualificationRequirement where
  parseJSON =
    Lude.withObject
      "QualificationRequirement"
      ( \x ->
          QualificationRequirement'
            Lude.<$> (x Lude..:? "LocaleValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ActionsGuarded")
            Lude.<*> (x Lude..:? "RequiredToPreview")
            Lude.<*> (x Lude..:? "IntegerValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "QualificationTypeId")
            Lude.<*> (x Lude..: "Comparator")
      )

instance Lude.ToJSON QualificationRequirement where
  toJSON QualificationRequirement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LocaleValues" Lude..=) Lude.<$> localeValues,
            ("ActionsGuarded" Lude..=) Lude.<$> actionsGuarded,
            ("RequiredToPreview" Lude..=) Lude.<$> requiredToPreview,
            ("IntegerValues" Lude..=) Lude.<$> integerValues,
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId),
            Lude.Just ("Comparator" Lude..= comparator)
          ]
      )
