{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignState where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignStatus
import Network.AWS.Prelude

-- | Provides information about the status of a campaign.
--
--
--
-- /See:/ 'campaignState' smart constructor.
newtype CampaignState = CampaignState'
  { _csCampaignStatus ::
      Maybe CampaignStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCampaignStatus' - The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign. If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
campaignState ::
  CampaignState
campaignState = CampaignState' {_csCampaignStatus = Nothing}

-- | The current status of the campaign, or the current status of a treatment that belongs to an A/B test campaign. If a campaign uses A/B testing, the campaign has a status of COMPLETED only if all campaign treatments have a status of COMPLETED. If you delete the segment that's associated with a campaign, the campaign fails and has a status of DELETED.
csCampaignStatus :: Lens' CampaignState (Maybe CampaignStatus)
csCampaignStatus = lens _csCampaignStatus (\s a -> s {_csCampaignStatus = a})

instance FromJSON CampaignState where
  parseJSON =
    withObject
      "CampaignState"
      (\x -> CampaignState' <$> (x .:? "CampaignStatus"))

instance Hashable CampaignState

instance NFData CampaignState
