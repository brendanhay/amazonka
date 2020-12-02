{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignResponse
import Network.AWS.Prelude

-- | Provides information about the configuration and other settings for all the campaigns that are associated with an application.
--
--
--
-- /See:/ 'campaignsResponse' smart constructor.
data CampaignsResponse = CampaignsResponse'
  { _cNextToken ::
      !(Maybe Text),
    _cItem :: ![CampaignResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'cItem' - An array of responses, one for each campaign that's associated with the application.
campaignsResponse ::
  CampaignsResponse
campaignsResponse =
  CampaignsResponse' {_cNextToken = Nothing, _cItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
cNextToken :: Lens' CampaignsResponse (Maybe Text)
cNextToken = lens _cNextToken (\s a -> s {_cNextToken = a})

-- | An array of responses, one for each campaign that's associated with the application.
cItem :: Lens' CampaignsResponse [CampaignResponse]
cItem = lens _cItem (\s a -> s {_cItem = a}) . _Coerce

instance FromJSON CampaignsResponse where
  parseJSON =
    withObject
      "CampaignsResponse"
      ( \x ->
          CampaignsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable CampaignsResponse

instance NFData CampaignsResponse
