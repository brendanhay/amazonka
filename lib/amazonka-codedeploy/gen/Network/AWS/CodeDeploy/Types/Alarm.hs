{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.Alarm where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an alarm.
--
--
--
-- /See:/ 'alarm' smart constructor.
newtype Alarm = Alarm' {_aName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aName' - The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
alarm ::
  Alarm
alarm = Alarm' {_aName = Nothing}

-- | The name of the alarm. Maximum length is 255 characters. Each alarm name can be used only once in a list of alarms.
aName :: Lens' Alarm (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

instance FromJSON Alarm where
  parseJSON = withObject "Alarm" (\x -> Alarm' <$> (x .:? "name"))

instance Hashable Alarm

instance NFData Alarm

instance ToJSON Alarm where
  toJSON Alarm' {..} = object (catMaybes [("name" .=) <$> _aName])
