{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FailoverCondition
import Network.AWS.MediaLive.Types.InputPreference
import Network.AWS.Prelude

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'automaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'
  { _aifsFailoverConditions ::
      !(Maybe [FailoverCondition]),
    _aifsErrorClearTimeMsec ::
      !(Maybe Nat),
    _aifsInputPreference ::
      !(Maybe InputPreference),
    _aifsSecondaryInputId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomaticInputFailoverSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifsFailoverConditions' - A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
--
-- * 'aifsErrorClearTimeMsec' - This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
--
-- * 'aifsInputPreference' - Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- * 'aifsSecondaryInputId' - The input ID of the secondary input in the automatic input failover pair.
automaticInputFailoverSettings ::
  -- | 'aifsSecondaryInputId'
  Text ->
  AutomaticInputFailoverSettings
automaticInputFailoverSettings pSecondaryInputId_ =
  AutomaticInputFailoverSettings'
    { _aifsFailoverConditions =
        Nothing,
      _aifsErrorClearTimeMsec = Nothing,
      _aifsInputPreference = Nothing,
      _aifsSecondaryInputId = pSecondaryInputId_
    }

-- | A list of failover conditions. If any of these conditions occur, MediaLive will perform a failover to the other input.
aifsFailoverConditions :: Lens' AutomaticInputFailoverSettings [FailoverCondition]
aifsFailoverConditions = lens _aifsFailoverConditions (\s a -> s {_aifsFailoverConditions = a}) . _Default . _Coerce

-- | This clear time defines the requirement a recovered input must meet to be considered healthy. The input must have no failover conditions for this length of time. Enter a time in milliseconds. This value is particularly important if the input_preference for the failover pair is set to PRIMARY_INPUT_PREFERRED, because after this time, MediaLive will switch back to the primary input.
aifsErrorClearTimeMsec :: Lens' AutomaticInputFailoverSettings (Maybe Natural)
aifsErrorClearTimeMsec = lens _aifsErrorClearTimeMsec (\s a -> s {_aifsErrorClearTimeMsec = a}) . mapping _Nat

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
aifsInputPreference :: Lens' AutomaticInputFailoverSettings (Maybe InputPreference)
aifsInputPreference = lens _aifsInputPreference (\s a -> s {_aifsInputPreference = a})

-- | The input ID of the secondary input in the automatic input failover pair.
aifsSecondaryInputId :: Lens' AutomaticInputFailoverSettings Text
aifsSecondaryInputId = lens _aifsSecondaryInputId (\s a -> s {_aifsSecondaryInputId = a})

instance FromJSON AutomaticInputFailoverSettings where
  parseJSON =
    withObject
      "AutomaticInputFailoverSettings"
      ( \x ->
          AutomaticInputFailoverSettings'
            <$> (x .:? "failoverConditions" .!= mempty)
            <*> (x .:? "errorClearTimeMsec")
            <*> (x .:? "inputPreference")
            <*> (x .: "secondaryInputId")
      )

instance Hashable AutomaticInputFailoverSettings

instance NFData AutomaticInputFailoverSettings

instance ToJSON AutomaticInputFailoverSettings where
  toJSON AutomaticInputFailoverSettings' {..} =
    object
      ( catMaybes
          [ ("failoverConditions" .=) <$> _aifsFailoverConditions,
            ("errorClearTimeMsec" .=) <$> _aifsErrorClearTimeMsec,
            ("inputPreference" .=) <$> _aifsInputPreference,
            Just ("secondaryInputId" .= _aifsSecondaryInputId)
          ]
      )
