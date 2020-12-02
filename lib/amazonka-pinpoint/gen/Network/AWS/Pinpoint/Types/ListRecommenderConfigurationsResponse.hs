{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
import Network.AWS.Prelude

-- | Provides information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
--
--
--
-- /See:/ 'listRecommenderConfigurationsResponse' smart constructor.
data ListRecommenderConfigurationsResponse = ListRecommenderConfigurationsResponse'
  { _lrcNextToken ::
      !(Maybe Text),
    _lrcItem ::
      ![RecommenderConfigurationResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRecommenderConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrcNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'lrcItem' - An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
listRecommenderConfigurationsResponse ::
  ListRecommenderConfigurationsResponse
listRecommenderConfigurationsResponse =
  ListRecommenderConfigurationsResponse'
    { _lrcNextToken = Nothing,
      _lrcItem = mempty
    }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
lrcNextToken :: Lens' ListRecommenderConfigurationsResponse (Maybe Text)
lrcNextToken = lens _lrcNextToken (\s a -> s {_lrcNextToken = a})

-- | An array of responses, one for each recommender model configuration that's associated with your Amazon Pinpoint account.
lrcItem :: Lens' ListRecommenderConfigurationsResponse [RecommenderConfigurationResponse]
lrcItem = lens _lrcItem (\s a -> s {_lrcItem = a}) . _Coerce

instance FromJSON ListRecommenderConfigurationsResponse where
  parseJSON =
    withObject
      "ListRecommenderConfigurationsResponse"
      ( \x ->
          ListRecommenderConfigurationsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable ListRecommenderConfigurationsResponse

instance NFData ListRecommenderConfigurationsResponse
