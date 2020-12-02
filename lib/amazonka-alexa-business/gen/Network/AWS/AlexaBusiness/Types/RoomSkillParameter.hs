{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomSkillParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A skill parameter associated with a room.
--
--
--
-- /See:/ 'roomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { _rspParameterKey ::
      !Text,
    _rspParameterValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoomSkillParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rspParameterKey' - The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- * 'rspParameterValue' - The parameter value of a room skill parameter.
roomSkillParameter ::
  -- | 'rspParameterKey'
  Text ->
  -- | 'rspParameterValue'
  Text ->
  RoomSkillParameter
roomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    { _rspParameterKey = pParameterKey_,
      _rspParameterValue = pParameterValue_
    }

-- | The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
rspParameterKey :: Lens' RoomSkillParameter Text
rspParameterKey = lens _rspParameterKey (\s a -> s {_rspParameterKey = a})

-- | The parameter value of a room skill parameter.
rspParameterValue :: Lens' RoomSkillParameter Text
rspParameterValue = lens _rspParameterValue (\s a -> s {_rspParameterValue = a})

instance FromJSON RoomSkillParameter where
  parseJSON =
    withObject
      "RoomSkillParameter"
      ( \x ->
          RoomSkillParameter'
            <$> (x .: "ParameterKey") <*> (x .: "ParameterValue")
      )

instance Hashable RoomSkillParameter

instance NFData RoomSkillParameter

instance ToJSON RoomSkillParameter where
  toJSON RoomSkillParameter' {..} =
    object
      ( catMaybes
          [ Just ("ParameterKey" .= _rspParameterKey),
            Just ("ParameterValue" .= _rspParameterValue)
          ]
      )
