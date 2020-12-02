{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ActivitiesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ActivitiesResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ActivityResponse
import Network.AWS.Prelude

-- | Provides information about the activities that were performed by a campaign.
--
--
--
-- /See:/ 'activitiesResponse' smart constructor.
data ActivitiesResponse = ActivitiesResponse'
  { _aNextToken ::
      !(Maybe Text),
    _aItem :: ![ActivityResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'aItem' - An array of responses, one for each activity that was performed by the campaign.
activitiesResponse ::
  ActivitiesResponse
activitiesResponse =
  ActivitiesResponse' {_aNextToken = Nothing, _aItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
aNextToken :: Lens' ActivitiesResponse (Maybe Text)
aNextToken = lens _aNextToken (\s a -> s {_aNextToken = a})

-- | An array of responses, one for each activity that was performed by the campaign.
aItem :: Lens' ActivitiesResponse [ActivityResponse]
aItem = lens _aItem (\s a -> s {_aItem = a}) . _Coerce

instance FromJSON ActivitiesResponse where
  parseJSON =
    withObject
      "ActivitiesResponse"
      ( \x ->
          ActivitiesResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable ActivitiesResponse

instance NFData ActivitiesResponse
