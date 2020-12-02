{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AncillarySourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Ancillary Source Settings
--
-- /See:/ 'ancillarySourceSettings' smart constructor.
newtype AncillarySourceSettings = AncillarySourceSettings'
  { _assSourceAncillaryChannelNumber ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AncillarySourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assSourceAncillaryChannelNumber' - Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
ancillarySourceSettings ::
  AncillarySourceSettings
ancillarySourceSettings =
  AncillarySourceSettings'
    { _assSourceAncillaryChannelNumber =
        Nothing
    }

-- | Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
assSourceAncillaryChannelNumber :: Lens' AncillarySourceSettings (Maybe Natural)
assSourceAncillaryChannelNumber = lens _assSourceAncillaryChannelNumber (\s a -> s {_assSourceAncillaryChannelNumber = a}) . mapping _Nat

instance FromJSON AncillarySourceSettings where
  parseJSON =
    withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            <$> (x .:? "sourceAncillaryChannelNumber")
      )

instance Hashable AncillarySourceSettings

instance NFData AncillarySourceSettings

instance ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    object
      ( catMaybes
          [ ("sourceAncillaryChannelNumber" .=)
              <$> _assSourceAncillaryChannelNumber
          ]
      )
