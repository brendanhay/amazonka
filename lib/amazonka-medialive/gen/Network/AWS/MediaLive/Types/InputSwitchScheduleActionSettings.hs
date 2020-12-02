{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import Network.AWS.Prelude

-- | Settings for the "switch input" action: to switch from ingesting one input to ingesting another input.
--
-- /See:/ 'inputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'
  { _issasInputClippingSettings ::
      !( Maybe
           InputClippingSettings
       ),
    _issasURLPath ::
      !(Maybe [Text]),
    _issasInputAttachmentNameReference ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSwitchScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issasInputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- * 'issasURLPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- * 'issasInputAttachmentNameReference' - The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
inputSwitchScheduleActionSettings ::
  -- | 'issasInputAttachmentNameReference'
  Text ->
  InputSwitchScheduleActionSettings
inputSwitchScheduleActionSettings pInputAttachmentNameReference_ =
  InputSwitchScheduleActionSettings'
    { _issasInputClippingSettings =
        Nothing,
      _issasURLPath = Nothing,
      _issasInputAttachmentNameReference =
        pInputAttachmentNameReference_
    }

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
issasInputClippingSettings :: Lens' InputSwitchScheduleActionSettings (Maybe InputClippingSettings)
issasInputClippingSettings = lens _issasInputClippingSettings (\s a -> s {_issasInputClippingSettings = a})

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
issasURLPath :: Lens' InputSwitchScheduleActionSettings [Text]
issasURLPath = lens _issasURLPath (\s a -> s {_issasURLPath = a}) . _Default . _Coerce

-- | The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
issasInputAttachmentNameReference :: Lens' InputSwitchScheduleActionSettings Text
issasInputAttachmentNameReference = lens _issasInputAttachmentNameReference (\s a -> s {_issasInputAttachmentNameReference = a})

instance FromJSON InputSwitchScheduleActionSettings where
  parseJSON =
    withObject
      "InputSwitchScheduleActionSettings"
      ( \x ->
          InputSwitchScheduleActionSettings'
            <$> (x .:? "inputClippingSettings")
            <*> (x .:? "urlPath" .!= mempty)
            <*> (x .: "inputAttachmentNameReference")
      )

instance Hashable InputSwitchScheduleActionSettings

instance NFData InputSwitchScheduleActionSettings

instance ToJSON InputSwitchScheduleActionSettings where
  toJSON InputSwitchScheduleActionSettings' {..} =
    object
      ( catMaybes
          [ ("inputClippingSettings" .=) <$> _issasInputClippingSettings,
            ("urlPath" .=) <$> _issasURLPath,
            Just
              ( "inputAttachmentNameReference"
                  .= _issasInputAttachmentNameReference
              )
          ]
      )
