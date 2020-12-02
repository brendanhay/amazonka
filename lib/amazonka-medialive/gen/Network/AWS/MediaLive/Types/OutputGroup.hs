{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroup where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Output
import Network.AWS.MediaLive.Types.OutputGroupSettings
import Network.AWS.Prelude

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'outputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { _ogName :: !(Maybe Text),
    _ogOutputs :: ![Output],
    _ogOutputGroupSettings :: !OutputGroupSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogName' - Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
--
-- * 'ogOutputs' - Undocumented member.
--
-- * 'ogOutputGroupSettings' - Settings associated with the output group.
outputGroup ::
  -- | 'ogOutputGroupSettings'
  OutputGroupSettings ->
  OutputGroup
outputGroup pOutputGroupSettings_ =
  OutputGroup'
    { _ogName = Nothing,
      _ogOutputs = mempty,
      _ogOutputGroupSettings = pOutputGroupSettings_
    }

-- | Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
ogName :: Lens' OutputGroup (Maybe Text)
ogName = lens _ogName (\s a -> s {_ogName = a})

-- | Undocumented member.
ogOutputs :: Lens' OutputGroup [Output]
ogOutputs = lens _ogOutputs (\s a -> s {_ogOutputs = a}) . _Coerce

-- | Settings associated with the output group.
ogOutputGroupSettings :: Lens' OutputGroup OutputGroupSettings
ogOutputGroupSettings = lens _ogOutputGroupSettings (\s a -> s {_ogOutputGroupSettings = a})

instance FromJSON OutputGroup where
  parseJSON =
    withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            <$> (x .:? "name")
            <*> (x .:? "outputs" .!= mempty)
            <*> (x .: "outputGroupSettings")
      )

instance Hashable OutputGroup

instance NFData OutputGroup

instance ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    object
      ( catMaybes
          [ ("name" .=) <$> _ogName,
            Just ("outputs" .= _ogOutputs),
            Just ("outputGroupSettings" .= _ogOutputGroupSettings)
          ]
      )
