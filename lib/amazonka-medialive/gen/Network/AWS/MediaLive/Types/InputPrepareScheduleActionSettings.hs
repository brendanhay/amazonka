{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import Network.AWS.Prelude

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'inputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'
  { _ipsasInputAttachmentNameReference ::
      !(Maybe Text),
    _ipsasInputClippingSettings ::
      !( Maybe
           InputClippingSettings
       ),
    _ipsasURLPath ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputPrepareScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsasInputAttachmentNameReference' - The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
--
-- * 'ipsasInputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- * 'ipsasURLPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
inputPrepareScheduleActionSettings ::
  InputPrepareScheduleActionSettings
inputPrepareScheduleActionSettings =
  InputPrepareScheduleActionSettings'
    { _ipsasInputAttachmentNameReference =
        Nothing,
      _ipsasInputClippingSettings = Nothing,
      _ipsasURLPath = Nothing
    }

-- | The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
ipsasInputAttachmentNameReference :: Lens' InputPrepareScheduleActionSettings (Maybe Text)
ipsasInputAttachmentNameReference = lens _ipsasInputAttachmentNameReference (\s a -> s {_ipsasInputAttachmentNameReference = a})

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
ipsasInputClippingSettings :: Lens' InputPrepareScheduleActionSettings (Maybe InputClippingSettings)
ipsasInputClippingSettings = lens _ipsasInputClippingSettings (\s a -> s {_ipsasInputClippingSettings = a})

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
ipsasURLPath :: Lens' InputPrepareScheduleActionSettings [Text]
ipsasURLPath = lens _ipsasURLPath (\s a -> s {_ipsasURLPath = a}) . _Default . _Coerce

instance FromJSON InputPrepareScheduleActionSettings where
  parseJSON =
    withObject
      "InputPrepareScheduleActionSettings"
      ( \x ->
          InputPrepareScheduleActionSettings'
            <$> (x .:? "inputAttachmentNameReference")
            <*> (x .:? "inputClippingSettings")
            <*> (x .:? "urlPath" .!= mempty)
      )

instance Hashable InputPrepareScheduleActionSettings

instance NFData InputPrepareScheduleActionSettings

instance ToJSON InputPrepareScheduleActionSettings where
  toJSON InputPrepareScheduleActionSettings' {..} =
    object
      ( catMaybes
          [ ("inputAttachmentNameReference" .=)
              <$> _ipsasInputAttachmentNameReference,
            ("inputClippingSettings" .=) <$> _ipsasInputClippingSettings,
            ("urlPath" .=) <$> _ipsasURLPath
          ]
      )
