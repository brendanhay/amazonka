{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillDetails where

import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Granular information about the skill.
--
--
--
-- /See:/ 'skillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { _sdSkillTypes :: !(Maybe [Text]),
    _sdProductDescription :: !(Maybe Text),
    _sdInvocationPhrase :: !(Maybe Text),
    _sdDeveloperInfo :: !(Maybe DeveloperInfo),
    _sdEndUserLicenseAgreement :: !(Maybe Text),
    _sdGenericKeywords :: !(Maybe [Text]),
    _sdReviews :: !(Maybe (Map Text (Text))),
    _sdReleaseDate :: !(Maybe Text),
    _sdNewInThisVersionBulletPoints :: !(Maybe [Text]),
    _sdBulletPoints :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SkillDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdSkillTypes' - The types of skills.
--
-- * 'sdProductDescription' - The description of the product.
--
-- * 'sdInvocationPhrase' - The phrase used to trigger the skill.
--
-- * 'sdDeveloperInfo' - The details about the developer that published the skill.
--
-- * 'sdEndUserLicenseAgreement' - The URL of the end user license agreement.
--
-- * 'sdGenericKeywords' - The generic keywords associated with the skill that can be used to find a skill.
--
-- * 'sdReviews' - /This member has been deprecated./  The list of reviews for the skill, including Key and Value pair.
--
-- * 'sdReleaseDate' - The date when the skill was released.
--
-- * 'sdNewInThisVersionBulletPoints' - The updates added in bullet points.
--
-- * 'sdBulletPoints' - The details about what the skill supports organized as bullet points.
skillDetails ::
  SkillDetails
skillDetails =
  SkillDetails'
    { _sdSkillTypes = Nothing,
      _sdProductDescription = Nothing,
      _sdInvocationPhrase = Nothing,
      _sdDeveloperInfo = Nothing,
      _sdEndUserLicenseAgreement = Nothing,
      _sdGenericKeywords = Nothing,
      _sdReviews = Nothing,
      _sdReleaseDate = Nothing,
      _sdNewInThisVersionBulletPoints = Nothing,
      _sdBulletPoints = Nothing
    }

-- | The types of skills.
sdSkillTypes :: Lens' SkillDetails [Text]
sdSkillTypes = lens _sdSkillTypes (\s a -> s {_sdSkillTypes = a}) . _Default . _Coerce

-- | The description of the product.
sdProductDescription :: Lens' SkillDetails (Maybe Text)
sdProductDescription = lens _sdProductDescription (\s a -> s {_sdProductDescription = a})

-- | The phrase used to trigger the skill.
sdInvocationPhrase :: Lens' SkillDetails (Maybe Text)
sdInvocationPhrase = lens _sdInvocationPhrase (\s a -> s {_sdInvocationPhrase = a})

-- | The details about the developer that published the skill.
sdDeveloperInfo :: Lens' SkillDetails (Maybe DeveloperInfo)
sdDeveloperInfo = lens _sdDeveloperInfo (\s a -> s {_sdDeveloperInfo = a})

-- | The URL of the end user license agreement.
sdEndUserLicenseAgreement :: Lens' SkillDetails (Maybe Text)
sdEndUserLicenseAgreement = lens _sdEndUserLicenseAgreement (\s a -> s {_sdEndUserLicenseAgreement = a})

-- | The generic keywords associated with the skill that can be used to find a skill.
sdGenericKeywords :: Lens' SkillDetails [Text]
sdGenericKeywords = lens _sdGenericKeywords (\s a -> s {_sdGenericKeywords = a}) . _Default . _Coerce

-- | /This member has been deprecated./  The list of reviews for the skill, including Key and Value pair.
sdReviews :: Lens' SkillDetails (HashMap Text (Text))
sdReviews = lens _sdReviews (\s a -> s {_sdReviews = a}) . _Default . _Map

-- | The date when the skill was released.
sdReleaseDate :: Lens' SkillDetails (Maybe Text)
sdReleaseDate = lens _sdReleaseDate (\s a -> s {_sdReleaseDate = a})

-- | The updates added in bullet points.
sdNewInThisVersionBulletPoints :: Lens' SkillDetails [Text]
sdNewInThisVersionBulletPoints = lens _sdNewInThisVersionBulletPoints (\s a -> s {_sdNewInThisVersionBulletPoints = a}) . _Default . _Coerce

-- | The details about what the skill supports organized as bullet points.
sdBulletPoints :: Lens' SkillDetails [Text]
sdBulletPoints = lens _sdBulletPoints (\s a -> s {_sdBulletPoints = a}) . _Default . _Coerce

instance FromJSON SkillDetails where
  parseJSON =
    withObject
      "SkillDetails"
      ( \x ->
          SkillDetails'
            <$> (x .:? "SkillTypes" .!= mempty)
            <*> (x .:? "ProductDescription")
            <*> (x .:? "InvocationPhrase")
            <*> (x .:? "DeveloperInfo")
            <*> (x .:? "EndUserLicenseAgreement")
            <*> (x .:? "GenericKeywords" .!= mempty)
            <*> (x .:? "Reviews" .!= mempty)
            <*> (x .:? "ReleaseDate")
            <*> (x .:? "NewInThisVersionBulletPoints" .!= mempty)
            <*> (x .:? "BulletPoints" .!= mempty)
      )

instance Hashable SkillDetails

instance NFData SkillDetails
