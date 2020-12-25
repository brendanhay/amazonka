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
    qrQualificationTypeId,
    qrComparator,
    qrActionsGuarded,
    qrIntegerValues,
    qrLocaleValues,
    qrRequiredToPreview,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.Comparator as Types
import qualified Network.AWS.MechanicalTurk.Types.HITAccessActions as Types
import qualified Network.AWS.MechanicalTurk.Types.Locale as Types
import qualified Network.AWS.MechanicalTurk.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | The QualificationRequirement data structure describes a Qualification that a Worker must have before the Worker is allowed to accept a HIT. A requirement may optionally state that a Worker must have the Qualification in order to preview the HIT, or see the HIT in search results.
--
-- /See:/ 'mkQualificationRequirement' smart constructor.
data QualificationRequirement = QualificationRequirement'
  { -- | The ID of the Qualification type for the requirement.
    qualificationTypeId :: Types.String,
    -- | The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
    comparator :: Types.Comparator,
    -- | Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
    actionsGuarded :: Core.Maybe Types.HITAccessActions,
    -- | The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
    integerValues :: Core.Maybe [Core.Int],
    -- | The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
    localeValues :: Core.Maybe [Types.Locale],
    -- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
    requiredToPreview :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QualificationRequirement' value with any optional fields omitted.
mkQualificationRequirement ::
  -- | 'qualificationTypeId'
  Types.String ->
  -- | 'comparator'
  Types.Comparator ->
  QualificationRequirement
mkQualificationRequirement qualificationTypeId comparator =
  QualificationRequirement'
    { qualificationTypeId,
      comparator,
      actionsGuarded = Core.Nothing,
      integerValues = Core.Nothing,
      localeValues = Core.Nothing,
      requiredToPreview = Core.Nothing
    }

-- | The ID of the Qualification type for the requirement.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrQualificationTypeId :: Lens.Lens' QualificationRequirement Types.String
qrQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED qrQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
--
-- /Note:/ Consider using 'comparator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrComparator :: Lens.Lens' QualificationRequirement Types.Comparator
qrComparator = Lens.field @"comparator"
{-# DEPRECATED qrComparator "Use generic-lens or generic-optics with 'comparator' instead." #-}

-- | Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
--
-- /Note:/ Consider using 'actionsGuarded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrActionsGuarded :: Lens.Lens' QualificationRequirement (Core.Maybe Types.HITAccessActions)
qrActionsGuarded = Lens.field @"actionsGuarded"
{-# DEPRECATED qrActionsGuarded "Use generic-lens or generic-optics with 'actionsGuarded' instead." #-}

-- | The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
--
-- /Note:/ Consider using 'integerValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrIntegerValues :: Lens.Lens' QualificationRequirement (Core.Maybe [Core.Int])
qrIntegerValues = Lens.field @"integerValues"
{-# DEPRECATED qrIntegerValues "Use generic-lens or generic-optics with 'integerValues' instead." #-}

-- | The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
--
-- /Note:/ Consider using 'localeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrLocaleValues :: Lens.Lens' QualificationRequirement (Core.Maybe [Types.Locale])
qrLocaleValues = Lens.field @"localeValues"
{-# DEPRECATED qrLocaleValues "Use generic-lens or generic-optics with 'localeValues' instead." #-}

-- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
--
-- /Note:/ Consider using 'requiredToPreview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrRequiredToPreview :: Lens.Lens' QualificationRequirement (Core.Maybe Core.Bool)
qrRequiredToPreview = Lens.field @"requiredToPreview"
{-# DEPRECATED qrRequiredToPreview "Use generic-lens or generic-optics with 'requiredToPreview' instead." #-}

instance Core.FromJSON QualificationRequirement where
  toJSON QualificationRequirement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            Core.Just ("Comparator" Core..= comparator),
            ("ActionsGuarded" Core..=) Core.<$> actionsGuarded,
            ("IntegerValues" Core..=) Core.<$> integerValues,
            ("LocaleValues" Core..=) Core.<$> localeValues,
            ("RequiredToPreview" Core..=) Core.<$> requiredToPreview
          ]
      )

instance Core.FromJSON QualificationRequirement where
  parseJSON =
    Core.withObject "QualificationRequirement" Core.$
      \x ->
        QualificationRequirement'
          Core.<$> (x Core..: "QualificationTypeId")
          Core.<*> (x Core..: "Comparator")
          Core.<*> (x Core..:? "ActionsGuarded")
          Core.<*> (x Core..:? "IntegerValues")
          Core.<*> (x Core..:? "LocaleValues")
          Core.<*> (x Core..:? "RequiredToPreview")
