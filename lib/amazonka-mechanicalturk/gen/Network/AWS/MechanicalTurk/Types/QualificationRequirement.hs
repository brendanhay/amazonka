{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequirement where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.Comparator
import Network.AWS.MechanicalTurk.Types.HITAccessActions
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.Prelude

-- | The QualificationRequirement data structure describes a Qualification that a Worker must have before the Worker is allowed to accept a HIT. A requirement may optionally state that a Worker must have the Qualification in order to preview the HIT, or see the HIT in search results.
--
--
--
-- /See:/ 'qualificationRequirement' smart constructor.
data QualificationRequirement = QualificationRequirement'
  { _qrLocaleValues ::
      !(Maybe [Locale]),
    _qrActionsGuarded ::
      !(Maybe HITAccessActions),
    _qrRequiredToPreview :: !(Maybe Bool),
    _qrIntegerValues :: !(Maybe [Int]),
    _qrQualificationTypeId :: !Text,
    _qrComparator :: !Comparator
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QualificationRequirement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrLocaleValues' - The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
--
-- * 'qrActionsGuarded' - Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
--
-- * 'qrRequiredToPreview' - DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
--
-- * 'qrIntegerValues' - The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
--
-- * 'qrQualificationTypeId' - The ID of the Qualification type for the requirement.
--
-- * 'qrComparator' - The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
qualificationRequirement ::
  -- | 'qrQualificationTypeId'
  Text ->
  -- | 'qrComparator'
  Comparator ->
  QualificationRequirement
qualificationRequirement pQualificationTypeId_ pComparator_ =
  QualificationRequirement'
    { _qrLocaleValues = Nothing,
      _qrActionsGuarded = Nothing,
      _qrRequiredToPreview = Nothing,
      _qrIntegerValues = Nothing,
      _qrQualificationTypeId = pQualificationTypeId_,
      _qrComparator = pComparator_
    }

-- | The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
qrLocaleValues :: Lens' QualificationRequirement [Locale]
qrLocaleValues = lens _qrLocaleValues (\s a -> s {_qrLocaleValues = a}) . _Default . _Coerce

-- | Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
qrActionsGuarded :: Lens' QualificationRequirement (Maybe HITAccessActions)
qrActionsGuarded = lens _qrActionsGuarded (\s a -> s {_qrActionsGuarded = a})

-- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
qrRequiredToPreview :: Lens' QualificationRequirement (Maybe Bool)
qrRequiredToPreview = lens _qrRequiredToPreview (\s a -> s {_qrRequiredToPreview = a})

-- | The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
qrIntegerValues :: Lens' QualificationRequirement [Int]
qrIntegerValues = lens _qrIntegerValues (\s a -> s {_qrIntegerValues = a}) . _Default . _Coerce

-- | The ID of the Qualification type for the requirement.
qrQualificationTypeId :: Lens' QualificationRequirement Text
qrQualificationTypeId = lens _qrQualificationTypeId (\s a -> s {_qrQualificationTypeId = a})

-- | The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
qrComparator :: Lens' QualificationRequirement Comparator
qrComparator = lens _qrComparator (\s a -> s {_qrComparator = a})

instance FromJSON QualificationRequirement where
  parseJSON =
    withObject
      "QualificationRequirement"
      ( \x ->
          QualificationRequirement'
            <$> (x .:? "LocaleValues" .!= mempty)
            <*> (x .:? "ActionsGuarded")
            <*> (x .:? "RequiredToPreview")
            <*> (x .:? "IntegerValues" .!= mempty)
            <*> (x .: "QualificationTypeId")
            <*> (x .: "Comparator")
      )

instance Hashable QualificationRequirement

instance NFData QualificationRequirement

instance ToJSON QualificationRequirement where
  toJSON QualificationRequirement' {..} =
    object
      ( catMaybes
          [ ("LocaleValues" .=) <$> _qrLocaleValues,
            ("ActionsGuarded" .=) <$> _qrActionsGuarded,
            ("RequiredToPreview" .=) <$> _qrRequiredToPreview,
            ("IntegerValues" .=) <$> _qrIntegerValues,
            Just ("QualificationTypeId" .= _qrQualificationTypeId),
            Just ("Comparator" .= _qrComparator)
          ]
      )
