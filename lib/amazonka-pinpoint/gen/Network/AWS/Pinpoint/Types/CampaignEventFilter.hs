{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEventFilter where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import Network.AWS.Prelude

-- | Specifies the settings for events that cause a campaign to be sent.
--
--
--
-- /See:/ 'campaignEventFilter' smart constructor.
data CampaignEventFilter = CampaignEventFilter'
  { _cefFilterType ::
      !FilterType,
    _cefDimensions :: !EventDimensions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignEventFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cefFilterType' - The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
--
-- * 'cefDimensions' - The dimension settings of the event filter for the campaign.
campaignEventFilter ::
  -- | 'cefFilterType'
  FilterType ->
  -- | 'cefDimensions'
  EventDimensions ->
  CampaignEventFilter
campaignEventFilter pFilterType_ pDimensions_ =
  CampaignEventFilter'
    { _cefFilterType = pFilterType_,
      _cefDimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
cefFilterType :: Lens' CampaignEventFilter FilterType
cefFilterType = lens _cefFilterType (\s a -> s {_cefFilterType = a})

-- | The dimension settings of the event filter for the campaign.
cefDimensions :: Lens' CampaignEventFilter EventDimensions
cefDimensions = lens _cefDimensions (\s a -> s {_cefDimensions = a})

instance FromJSON CampaignEventFilter where
  parseJSON =
    withObject
      "CampaignEventFilter"
      ( \x ->
          CampaignEventFilter'
            <$> (x .: "FilterType") <*> (x .: "Dimensions")
      )

instance Hashable CampaignEventFilter

instance NFData CampaignEventFilter

instance ToJSON CampaignEventFilter where
  toJSON CampaignEventFilter' {..} =
    object
      ( catMaybes
          [ Just ("FilterType" .= _cefFilterType),
            Just ("Dimensions" .= _cefDimensions)
          ]
      )
