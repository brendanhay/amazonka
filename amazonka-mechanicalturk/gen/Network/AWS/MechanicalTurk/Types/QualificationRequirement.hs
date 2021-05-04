{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequirement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequirement where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.Comparator
import Network.AWS.MechanicalTurk.Types.HITAccessActions
import Network.AWS.MechanicalTurk.Types.Locale
import qualified Network.AWS.Prelude as Prelude

-- | The QualificationRequirement data structure describes a Qualification
-- that a Worker must have before the Worker is allowed to accept a HIT. A
-- requirement may optionally state that a Worker must have the
-- Qualification in order to preview the HIT, or see the HIT in search
-- results.
--
-- /See:/ 'newQualificationRequirement' smart constructor.
data QualificationRequirement = QualificationRequirement'
  { -- | Setting this attribute prevents Workers whose Qualifications do not meet
    -- this QualificationRequirement from taking the specified action. Valid
    -- arguments include \"Accept\" (Worker cannot accept the HIT, but can
    -- preview the HIT and see it in their search results),
    -- \"PreviewAndAccept\" (Worker cannot accept or preview the HIT, but can
    -- see the HIT in their search results), and \"DiscoverPreviewAndAccept\"
    -- (Worker cannot accept, preview, or see the HIT in their search results).
    -- It\'s possible for you to create a HIT with multiple
    -- QualificationRequirements (which can have different values for the
    -- ActionGuarded attribute). In this case, the Worker is only permitted to
    -- perform an action when they have met all QualificationRequirements
    -- guarding the action. The actions in the order of least restrictive to
    -- most restrictive are Discover, Preview and Accept. For example, if a
    -- Worker meets all QualificationRequirements that are set to
    -- DiscoverPreviewAndAccept, but do not meet all requirements that are set
    -- with PreviewAndAccept, then the Worker will be able to Discover, i.e.
    -- see the HIT in their search result, but will not be able to Preview or
    -- Accept the HIT. ActionsGuarded should not be used in combination with
    -- the @RequiredToPreview@ field.
    actionsGuarded :: Prelude.Maybe HITAccessActions,
    -- | The locale value to compare against the Qualification\'s value. The
    -- local value must be a valid ISO 3166 country code or supports ISO 3166-2
    -- subdivisions. LocaleValue can only be used with a Worker_Locale
    -- QualificationType ID. LocaleValue can only be used with the EqualTo,
    -- NotEqualTo, In, and NotIn comparators. You must only use a single
    -- LocaleValue element when using the EqualTo or NotEqualTo comparators.
    -- When performing a set comparison by using the In or the NotIn
    -- comparator, you can use up to 30 LocaleValue elements in a
    -- QualificationRequirement data structure.
    localeValues :: Prelude.Maybe [Locale],
    -- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview
    -- is true, the question data for the HIT will not be shown when a Worker
    -- whose Qualifications do not meet this requirement tries to preview the
    -- HIT. That is, a Worker\'s Qualifications must meet all of the
    -- requirements for which RequiredToPreview is true in order to preview the
    -- HIT. If a Worker meets all of the requirements where RequiredToPreview
    -- is true (or if there are no such requirements), but does not meet all of
    -- the requirements for the HIT, the Worker will be allowed to preview the
    -- HIT\'s question data, but will not be allowed to accept and complete the
    -- HIT. The default is false. This should not be used in combination with
    -- the @ActionsGuarded@ field.
    requiredToPreview :: Prelude.Maybe Prelude.Bool,
    -- | The integer value to compare against the Qualification\'s value.
    -- IntegerValue must not be present if Comparator is Exists or
    -- DoesNotExist. IntegerValue can only be used if the Qualification type
    -- has an integer value; it cannot be used with the Worker_Locale
    -- QualificationType ID. When performing a set comparison by using the In
    -- or the NotIn comparator, you can use up to 15 IntegerValue elements in a
    -- QualificationRequirement data structure.
    integerValues :: Prelude.Maybe [Prelude.Int],
    -- | The ID of the Qualification type for the requirement.
    qualificationTypeId :: Prelude.Text,
    -- | The kind of comparison to make against a Qualification\'s value. You can
    -- compare a Qualification\'s value to an IntegerValue to see if it is
    -- LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo,
    -- or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to
    -- see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to
    -- see if the value is In or NotIn a set of IntegerValue or LocaleValue
    -- values. Lastly, a Qualification requirement can also test if a
    -- Qualification Exists or DoesNotExist in the user\'s profile, regardless
    -- of its value.
    comparator :: Comparator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QualificationRequirement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionsGuarded', 'qualificationRequirement_actionsGuarded' - Setting this attribute prevents Workers whose Qualifications do not meet
-- this QualificationRequirement from taking the specified action. Valid
-- arguments include \"Accept\" (Worker cannot accept the HIT, but can
-- preview the HIT and see it in their search results),
-- \"PreviewAndAccept\" (Worker cannot accept or preview the HIT, but can
-- see the HIT in their search results), and \"DiscoverPreviewAndAccept\"
-- (Worker cannot accept, preview, or see the HIT in their search results).
-- It\'s possible for you to create a HIT with multiple
-- QualificationRequirements (which can have different values for the
-- ActionGuarded attribute). In this case, the Worker is only permitted to
-- perform an action when they have met all QualificationRequirements
-- guarding the action. The actions in the order of least restrictive to
-- most restrictive are Discover, Preview and Accept. For example, if a
-- Worker meets all QualificationRequirements that are set to
-- DiscoverPreviewAndAccept, but do not meet all requirements that are set
-- with PreviewAndAccept, then the Worker will be able to Discover, i.e.
-- see the HIT in their search result, but will not be able to Preview or
-- Accept the HIT. ActionsGuarded should not be used in combination with
-- the @RequiredToPreview@ field.
--
-- 'localeValues', 'qualificationRequirement_localeValues' - The locale value to compare against the Qualification\'s value. The
-- local value must be a valid ISO 3166 country code or supports ISO 3166-2
-- subdivisions. LocaleValue can only be used with a Worker_Locale
-- QualificationType ID. LocaleValue can only be used with the EqualTo,
-- NotEqualTo, In, and NotIn comparators. You must only use a single
-- LocaleValue element when using the EqualTo or NotEqualTo comparators.
-- When performing a set comparison by using the In or the NotIn
-- comparator, you can use up to 30 LocaleValue elements in a
-- QualificationRequirement data structure.
--
-- 'requiredToPreview', 'qualificationRequirement_requiredToPreview' - DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview
-- is true, the question data for the HIT will not be shown when a Worker
-- whose Qualifications do not meet this requirement tries to preview the
-- HIT. That is, a Worker\'s Qualifications must meet all of the
-- requirements for which RequiredToPreview is true in order to preview the
-- HIT. If a Worker meets all of the requirements where RequiredToPreview
-- is true (or if there are no such requirements), but does not meet all of
-- the requirements for the HIT, the Worker will be allowed to preview the
-- HIT\'s question data, but will not be allowed to accept and complete the
-- HIT. The default is false. This should not be used in combination with
-- the @ActionsGuarded@ field.
--
-- 'integerValues', 'qualificationRequirement_integerValues' - The integer value to compare against the Qualification\'s value.
-- IntegerValue must not be present if Comparator is Exists or
-- DoesNotExist. IntegerValue can only be used if the Qualification type
-- has an integer value; it cannot be used with the Worker_Locale
-- QualificationType ID. When performing a set comparison by using the In
-- or the NotIn comparator, you can use up to 15 IntegerValue elements in a
-- QualificationRequirement data structure.
--
-- 'qualificationTypeId', 'qualificationRequirement_qualificationTypeId' - The ID of the Qualification type for the requirement.
--
-- 'comparator', 'qualificationRequirement_comparator' - The kind of comparison to make against a Qualification\'s value. You can
-- compare a Qualification\'s value to an IntegerValue to see if it is
-- LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo,
-- or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to
-- see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to
-- see if the value is In or NotIn a set of IntegerValue or LocaleValue
-- values. Lastly, a Qualification requirement can also test if a
-- Qualification Exists or DoesNotExist in the user\'s profile, regardless
-- of its value.
newQualificationRequirement ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  -- | 'comparator'
  Comparator ->
  QualificationRequirement
newQualificationRequirement
  pQualificationTypeId_
  pComparator_ =
    QualificationRequirement'
      { actionsGuarded =
          Prelude.Nothing,
        localeValues = Prelude.Nothing,
        requiredToPreview = Prelude.Nothing,
        integerValues = Prelude.Nothing,
        qualificationTypeId = pQualificationTypeId_,
        comparator = pComparator_
      }

-- | Setting this attribute prevents Workers whose Qualifications do not meet
-- this QualificationRequirement from taking the specified action. Valid
-- arguments include \"Accept\" (Worker cannot accept the HIT, but can
-- preview the HIT and see it in their search results),
-- \"PreviewAndAccept\" (Worker cannot accept or preview the HIT, but can
-- see the HIT in their search results), and \"DiscoverPreviewAndAccept\"
-- (Worker cannot accept, preview, or see the HIT in their search results).
-- It\'s possible for you to create a HIT with multiple
-- QualificationRequirements (which can have different values for the
-- ActionGuarded attribute). In this case, the Worker is only permitted to
-- perform an action when they have met all QualificationRequirements
-- guarding the action. The actions in the order of least restrictive to
-- most restrictive are Discover, Preview and Accept. For example, if a
-- Worker meets all QualificationRequirements that are set to
-- DiscoverPreviewAndAccept, but do not meet all requirements that are set
-- with PreviewAndAccept, then the Worker will be able to Discover, i.e.
-- see the HIT in their search result, but will not be able to Preview or
-- Accept the HIT. ActionsGuarded should not be used in combination with
-- the @RequiredToPreview@ field.
qualificationRequirement_actionsGuarded :: Lens.Lens' QualificationRequirement (Prelude.Maybe HITAccessActions)
qualificationRequirement_actionsGuarded = Lens.lens (\QualificationRequirement' {actionsGuarded} -> actionsGuarded) (\s@QualificationRequirement' {} a -> s {actionsGuarded = a} :: QualificationRequirement)

-- | The locale value to compare against the Qualification\'s value. The
-- local value must be a valid ISO 3166 country code or supports ISO 3166-2
-- subdivisions. LocaleValue can only be used with a Worker_Locale
-- QualificationType ID. LocaleValue can only be used with the EqualTo,
-- NotEqualTo, In, and NotIn comparators. You must only use a single
-- LocaleValue element when using the EqualTo or NotEqualTo comparators.
-- When performing a set comparison by using the In or the NotIn
-- comparator, you can use up to 30 LocaleValue elements in a
-- QualificationRequirement data structure.
qualificationRequirement_localeValues :: Lens.Lens' QualificationRequirement (Prelude.Maybe [Locale])
qualificationRequirement_localeValues = Lens.lens (\QualificationRequirement' {localeValues} -> localeValues) (\s@QualificationRequirement' {} a -> s {localeValues = a} :: QualificationRequirement) Prelude.. Lens.mapping Prelude._Coerce

-- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview
-- is true, the question data for the HIT will not be shown when a Worker
-- whose Qualifications do not meet this requirement tries to preview the
-- HIT. That is, a Worker\'s Qualifications must meet all of the
-- requirements for which RequiredToPreview is true in order to preview the
-- HIT. If a Worker meets all of the requirements where RequiredToPreview
-- is true (or if there are no such requirements), but does not meet all of
-- the requirements for the HIT, the Worker will be allowed to preview the
-- HIT\'s question data, but will not be allowed to accept and complete the
-- HIT. The default is false. This should not be used in combination with
-- the @ActionsGuarded@ field.
qualificationRequirement_requiredToPreview :: Lens.Lens' QualificationRequirement (Prelude.Maybe Prelude.Bool)
qualificationRequirement_requiredToPreview = Lens.lens (\QualificationRequirement' {requiredToPreview} -> requiredToPreview) (\s@QualificationRequirement' {} a -> s {requiredToPreview = a} :: QualificationRequirement)

-- | The integer value to compare against the Qualification\'s value.
-- IntegerValue must not be present if Comparator is Exists or
-- DoesNotExist. IntegerValue can only be used if the Qualification type
-- has an integer value; it cannot be used with the Worker_Locale
-- QualificationType ID. When performing a set comparison by using the In
-- or the NotIn comparator, you can use up to 15 IntegerValue elements in a
-- QualificationRequirement data structure.
qualificationRequirement_integerValues :: Lens.Lens' QualificationRequirement (Prelude.Maybe [Prelude.Int])
qualificationRequirement_integerValues = Lens.lens (\QualificationRequirement' {integerValues} -> integerValues) (\s@QualificationRequirement' {} a -> s {integerValues = a} :: QualificationRequirement) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the Qualification type for the requirement.
qualificationRequirement_qualificationTypeId :: Lens.Lens' QualificationRequirement Prelude.Text
qualificationRequirement_qualificationTypeId = Lens.lens (\QualificationRequirement' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationRequirement' {} a -> s {qualificationTypeId = a} :: QualificationRequirement)

-- | The kind of comparison to make against a Qualification\'s value. You can
-- compare a Qualification\'s value to an IntegerValue to see if it is
-- LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo,
-- or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to
-- see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to
-- see if the value is In or NotIn a set of IntegerValue or LocaleValue
-- values. Lastly, a Qualification requirement can also test if a
-- Qualification Exists or DoesNotExist in the user\'s profile, regardless
-- of its value.
qualificationRequirement_comparator :: Lens.Lens' QualificationRequirement Comparator
qualificationRequirement_comparator = Lens.lens (\QualificationRequirement' {comparator} -> comparator) (\s@QualificationRequirement' {} a -> s {comparator = a} :: QualificationRequirement)

instance Prelude.FromJSON QualificationRequirement where
  parseJSON =
    Prelude.withObject
      "QualificationRequirement"
      ( \x ->
          QualificationRequirement'
            Prelude.<$> (x Prelude..:? "ActionsGuarded")
            Prelude.<*> ( x Prelude..:? "LocaleValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "RequiredToPreview")
            Prelude.<*> ( x Prelude..:? "IntegerValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "QualificationTypeId")
            Prelude.<*> (x Prelude..: "Comparator")
      )

instance Prelude.Hashable QualificationRequirement

instance Prelude.NFData QualificationRequirement

instance Prelude.ToJSON QualificationRequirement where
  toJSON QualificationRequirement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ActionsGuarded" Prelude..=)
              Prelude.<$> actionsGuarded,
            ("LocaleValues" Prelude..=) Prelude.<$> localeValues,
            ("RequiredToPreview" Prelude..=)
              Prelude.<$> requiredToPreview,
            ("IntegerValues" Prelude..=)
              Prelude.<$> integerValues,
            Prelude.Just
              ( "QualificationTypeId"
                  Prelude..= qualificationTypeId
              ),
            Prelude.Just ("Comparator" Prelude..= comparator)
          ]
      )
