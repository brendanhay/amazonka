{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProvider where

import Network.AWS.AlexaBusiness.Types.ConferenceProviderType
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entity that provides a conferencing solution. Alexa for Business acts as the voice interface and mediator that connects users to their preferred conference provider. Examples of conference providers include Amazon Chime, Zoom, Cisco, and Polycom.
--
--
--
-- /See:/ 'conferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { _cpMeetingSetting ::
      !(Maybe MeetingSetting),
    _cpARN :: !(Maybe Text),
    _cpPSTNDialIn :: !(Maybe PSTNDialIn),
    _cpName :: !(Maybe Text),
    _cpType :: !(Maybe ConferenceProviderType),
    _cpIPDialIn :: !(Maybe IPDialIn)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpMeetingSetting' - The meeting settings for the conference provider.
--
-- * 'cpARN' - The ARN of the newly created conference provider.
--
-- * 'cpPSTNDialIn' - The information for PSTN conferencing.
--
-- * 'cpName' - The name of the conference provider.
--
-- * 'cpType' - The type of conference providers.
--
-- * 'cpIPDialIn' - The IP endpoint and protocol for calling.
conferenceProvider ::
  ConferenceProvider
conferenceProvider =
  ConferenceProvider'
    { _cpMeetingSetting = Nothing,
      _cpARN = Nothing,
      _cpPSTNDialIn = Nothing,
      _cpName = Nothing,
      _cpType = Nothing,
      _cpIPDialIn = Nothing
    }

-- | The meeting settings for the conference provider.
cpMeetingSetting :: Lens' ConferenceProvider (Maybe MeetingSetting)
cpMeetingSetting = lens _cpMeetingSetting (\s a -> s {_cpMeetingSetting = a})

-- | The ARN of the newly created conference provider.
cpARN :: Lens' ConferenceProvider (Maybe Text)
cpARN = lens _cpARN (\s a -> s {_cpARN = a})

-- | The information for PSTN conferencing.
cpPSTNDialIn :: Lens' ConferenceProvider (Maybe PSTNDialIn)
cpPSTNDialIn = lens _cpPSTNDialIn (\s a -> s {_cpPSTNDialIn = a})

-- | The name of the conference provider.
cpName :: Lens' ConferenceProvider (Maybe Text)
cpName = lens _cpName (\s a -> s {_cpName = a})

-- | The type of conference providers.
cpType :: Lens' ConferenceProvider (Maybe ConferenceProviderType)
cpType = lens _cpType (\s a -> s {_cpType = a})

-- | The IP endpoint and protocol for calling.
cpIPDialIn :: Lens' ConferenceProvider (Maybe IPDialIn)
cpIPDialIn = lens _cpIPDialIn (\s a -> s {_cpIPDialIn = a})

instance FromJSON ConferenceProvider where
  parseJSON =
    withObject
      "ConferenceProvider"
      ( \x ->
          ConferenceProvider'
            <$> (x .:? "MeetingSetting")
            <*> (x .:? "Arn")
            <*> (x .:? "PSTNDialIn")
            <*> (x .:? "Name")
            <*> (x .:? "Type")
            <*> (x .:? "IPDialIn")
      )

instance Hashable ConferenceProvider

instance NFData ConferenceProvider
