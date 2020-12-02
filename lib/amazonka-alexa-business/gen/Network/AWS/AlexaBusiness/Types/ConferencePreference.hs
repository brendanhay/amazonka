{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferencePreference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The default conference provider that is used if no other scheduled meetings are detected.
--
--
--
-- /See:/ 'conferencePreference' smart constructor.
newtype ConferencePreference = ConferencePreference'
  { _cpDefaultConferenceProviderARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConferencePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpDefaultConferenceProviderARN' - The ARN of the default conference provider.
conferencePreference ::
  ConferencePreference
conferencePreference =
  ConferencePreference' {_cpDefaultConferenceProviderARN = Nothing}

-- | The ARN of the default conference provider.
cpDefaultConferenceProviderARN :: Lens' ConferencePreference (Maybe Text)
cpDefaultConferenceProviderARN = lens _cpDefaultConferenceProviderARN (\s a -> s {_cpDefaultConferenceProviderARN = a})

instance FromJSON ConferencePreference where
  parseJSON =
    withObject
      "ConferencePreference"
      ( \x ->
          ConferencePreference' <$> (x .:? "DefaultConferenceProviderArn")
      )

instance Hashable ConferencePreference

instance NFData ConferencePreference

instance ToJSON ConferencePreference where
  toJSON ConferencePreference' {..} =
    object
      ( catMaybes
          [ ("DefaultConferenceProviderArn" .=)
              <$> _cpDefaultConferenceProviderARN
          ]
      )
