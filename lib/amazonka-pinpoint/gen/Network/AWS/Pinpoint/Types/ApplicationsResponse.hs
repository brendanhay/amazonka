{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ApplicationResponse
import Network.AWS.Prelude

-- | Provides information about all of your applications.
--
--
--
-- /See:/ 'applicationsResponse' smart constructor.
data ApplicationsResponse = ApplicationsResponse'
  { _appNextToken ::
      !(Maybe Text),
    _appItem :: !(Maybe [ApplicationResponse])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'appItem' - An array of responses, one for each application that was returned.
applicationsResponse ::
  ApplicationsResponse
applicationsResponse =
  ApplicationsResponse'
    { _appNextToken = Nothing,
      _appItem = Nothing
    }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
appNextToken :: Lens' ApplicationsResponse (Maybe Text)
appNextToken = lens _appNextToken (\s a -> s {_appNextToken = a})

-- | An array of responses, one for each application that was returned.
appItem :: Lens' ApplicationsResponse [ApplicationResponse]
appItem = lens _appItem (\s a -> s {_appItem = a}) . _Default . _Coerce

instance FromJSON ApplicationsResponse where
  parseJSON =
    withObject
      "ApplicationsResponse"
      ( \x ->
          ApplicationsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable ApplicationsResponse

instance NFData ApplicationsResponse
