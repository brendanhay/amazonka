{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputAttachment where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
import Network.AWS.MediaLive.Types.InputSettings
import Network.AWS.Prelude

-- | Placeholder documentation for InputAttachment
--
-- /See:/ 'inputAttachment' smart constructor.
data InputAttachment = InputAttachment'
  { _iaInputAttachmentName ::
      !(Maybe Text),
    _iaInputId :: !(Maybe Text),
    _iaAutomaticInputFailoverSettings ::
      !(Maybe AutomaticInputFailoverSettings),
    _iaInputSettings :: !(Maybe InputSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaInputAttachmentName' - User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
--
-- * 'iaInputId' - The ID of the input
--
-- * 'iaAutomaticInputFailoverSettings' - User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
--
-- * 'iaInputSettings' - Settings of an input (caption selector, etc.)
inputAttachment ::
  InputAttachment
inputAttachment =
  InputAttachment'
    { _iaInputAttachmentName = Nothing,
      _iaInputId = Nothing,
      _iaAutomaticInputFailoverSettings = Nothing,
      _iaInputSettings = Nothing
    }

-- | User-specified name for the attachment. This is required if the user wants to use this input in an input switch action.
iaInputAttachmentName :: Lens' InputAttachment (Maybe Text)
iaInputAttachmentName = lens _iaInputAttachmentName (\s a -> s {_iaInputAttachmentName = a})

-- | The ID of the input
iaInputId :: Lens' InputAttachment (Maybe Text)
iaInputId = lens _iaInputId (\s a -> s {_iaInputId = a})

-- | User-specified settings for defining what the conditions are for declaring the input unhealthy and failing over to a different input.
iaAutomaticInputFailoverSettings :: Lens' InputAttachment (Maybe AutomaticInputFailoverSettings)
iaAutomaticInputFailoverSettings = lens _iaAutomaticInputFailoverSettings (\s a -> s {_iaAutomaticInputFailoverSettings = a})

-- | Settings of an input (caption selector, etc.)
iaInputSettings :: Lens' InputAttachment (Maybe InputSettings)
iaInputSettings = lens _iaInputSettings (\s a -> s {_iaInputSettings = a})

instance FromJSON InputAttachment where
  parseJSON =
    withObject
      "InputAttachment"
      ( \x ->
          InputAttachment'
            <$> (x .:? "inputAttachmentName")
            <*> (x .:? "inputId")
            <*> (x .:? "automaticInputFailoverSettings")
            <*> (x .:? "inputSettings")
      )

instance Hashable InputAttachment

instance NFData InputAttachment

instance ToJSON InputAttachment where
  toJSON InputAttachment' {..} =
    object
      ( catMaybes
          [ ("inputAttachmentName" .=) <$> _iaInputAttachmentName,
            ("inputId" .=) <$> _iaInputId,
            ("automaticInputFailoverSettings" .=)
              <$> _iaAutomaticInputFailoverSettings,
            ("inputSettings" .=) <$> _iaInputSettings
          ]
      )
