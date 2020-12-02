{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillsStoreSkill where

import Network.AWS.AlexaBusiness.Types.SkillDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The detailed information about an Alexa skill.
--
--
--
-- /See:/ 'skillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { _sssSkillId ::
      !(Maybe Text),
    _sssSupportsLinking :: !(Maybe Bool),
    _sssSampleUtterances :: !(Maybe [Text]),
    _sssShortDescription :: !(Maybe Text),
    _sssIconURL :: !(Maybe Text),
    _sssSkillDetails :: !(Maybe SkillDetails),
    _sssSkillName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SkillsStoreSkill' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssSkillId' - The ARN of the skill.
--
-- * 'sssSupportsLinking' - Linking support for a skill.
--
-- * 'sssSampleUtterances' - Sample utterances that interact with the skill.
--
-- * 'sssShortDescription' - Short description about the skill.
--
-- * 'sssIconURL' - The URL where the skill icon resides.
--
-- * 'sssSkillDetails' - Information about the skill.
--
-- * 'sssSkillName' - The name of the skill.
skillsStoreSkill ::
  SkillsStoreSkill
skillsStoreSkill =
  SkillsStoreSkill'
    { _sssSkillId = Nothing,
      _sssSupportsLinking = Nothing,
      _sssSampleUtterances = Nothing,
      _sssShortDescription = Nothing,
      _sssIconURL = Nothing,
      _sssSkillDetails = Nothing,
      _sssSkillName = Nothing
    }

-- | The ARN of the skill.
sssSkillId :: Lens' SkillsStoreSkill (Maybe Text)
sssSkillId = lens _sssSkillId (\s a -> s {_sssSkillId = a})

-- | Linking support for a skill.
sssSupportsLinking :: Lens' SkillsStoreSkill (Maybe Bool)
sssSupportsLinking = lens _sssSupportsLinking (\s a -> s {_sssSupportsLinking = a})

-- | Sample utterances that interact with the skill.
sssSampleUtterances :: Lens' SkillsStoreSkill [Text]
sssSampleUtterances = lens _sssSampleUtterances (\s a -> s {_sssSampleUtterances = a}) . _Default . _Coerce

-- | Short description about the skill.
sssShortDescription :: Lens' SkillsStoreSkill (Maybe Text)
sssShortDescription = lens _sssShortDescription (\s a -> s {_sssShortDescription = a})

-- | The URL where the skill icon resides.
sssIconURL :: Lens' SkillsStoreSkill (Maybe Text)
sssIconURL = lens _sssIconURL (\s a -> s {_sssIconURL = a})

-- | Information about the skill.
sssSkillDetails :: Lens' SkillsStoreSkill (Maybe SkillDetails)
sssSkillDetails = lens _sssSkillDetails (\s a -> s {_sssSkillDetails = a})

-- | The name of the skill.
sssSkillName :: Lens' SkillsStoreSkill (Maybe Text)
sssSkillName = lens _sssSkillName (\s a -> s {_sssSkillName = a})

instance FromJSON SkillsStoreSkill where
  parseJSON =
    withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            <$> (x .:? "SkillId")
            <*> (x .:? "SupportsLinking")
            <*> (x .:? "SampleUtterances" .!= mempty)
            <*> (x .:? "ShortDescription")
            <*> (x .:? "IconUrl")
            <*> (x .:? "SkillDetails")
            <*> (x .:? "SkillName")
      )

instance Hashable SkillsStoreSkill

instance NFData SkillsStoreSkill
