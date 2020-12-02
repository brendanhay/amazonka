{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeveloperInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeveloperInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details about the developer that published the skill.
--
--
--
-- /See:/ 'developerInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { _diEmail :: !(Maybe Text),
    _diURL :: !(Maybe Text),
    _diPrivacyPolicy :: !(Maybe Text),
    _diDeveloperName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeveloperInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEmail' - The email of the developer.
--
-- * 'diURL' - The website of the developer.
--
-- * 'diPrivacyPolicy' - The URL of the privacy policy.
--
-- * 'diDeveloperName' - The name of the developer.
developerInfo ::
  DeveloperInfo
developerInfo =
  DeveloperInfo'
    { _diEmail = Nothing,
      _diURL = Nothing,
      _diPrivacyPolicy = Nothing,
      _diDeveloperName = Nothing
    }

-- | The email of the developer.
diEmail :: Lens' DeveloperInfo (Maybe Text)
diEmail = lens _diEmail (\s a -> s {_diEmail = a})

-- | The website of the developer.
diURL :: Lens' DeveloperInfo (Maybe Text)
diURL = lens _diURL (\s a -> s {_diURL = a})

-- | The URL of the privacy policy.
diPrivacyPolicy :: Lens' DeveloperInfo (Maybe Text)
diPrivacyPolicy = lens _diPrivacyPolicy (\s a -> s {_diPrivacyPolicy = a})

-- | The name of the developer.
diDeveloperName :: Lens' DeveloperInfo (Maybe Text)
diDeveloperName = lens _diDeveloperName (\s a -> s {_diDeveloperName = a})

instance FromJSON DeveloperInfo where
  parseJSON =
    withObject
      "DeveloperInfo"
      ( \x ->
          DeveloperInfo'
            <$> (x .:? "Email")
            <*> (x .:? "Url")
            <*> (x .:? "PrivacyPolicy")
            <*> (x .:? "DeveloperName")
      )

instance Hashable DeveloperInfo

instance NFData DeveloperInfo
