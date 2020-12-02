{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignCustomMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignCustomMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the contents of a message that's sent through a custom channel to recipients of a campaign.
--
--
--
-- /See:/ 'campaignCustomMessage' smart constructor.
newtype CampaignCustomMessage = CampaignCustomMessage'
  { _ccmData ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignCustomMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccmData' - The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
campaignCustomMessage ::
  CampaignCustomMessage
campaignCustomMessage = CampaignCustomMessage' {_ccmData = Nothing}

-- | The raw, JSON-formatted string to use as the payload for the message. The maximum size is 5 KB.
ccmData :: Lens' CampaignCustomMessage (Maybe Text)
ccmData = lens _ccmData (\s a -> s {_ccmData = a})

instance FromJSON CampaignCustomMessage where
  parseJSON =
    withObject
      "CampaignCustomMessage"
      (\x -> CampaignCustomMessage' <$> (x .:? "Data"))

instance Hashable CampaignCustomMessage

instance NFData CampaignCustomMessage

instance ToJSON CampaignCustomMessage where
  toJSON CampaignCustomMessage' {..} =
    object (catMaybes [("Data" .=) <$> _ccmData])
