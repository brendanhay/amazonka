{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneysResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneysResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.JourneyResponse
import Network.AWS.Prelude

-- | Provides information about the status, configuration, and other settings for all the journeys that are associated with an application.
--
--
--
-- /See:/ 'journeysResponse' smart constructor.
data JourneysResponse = JourneysResponse'
  { _jNextToken ::
      !(Maybe Text),
    _jItem :: ![JourneyResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'jItem' - An array of responses, one for each journey that's associated with the application.
journeysResponse ::
  JourneysResponse
journeysResponse =
  JourneysResponse' {_jNextToken = Nothing, _jItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
jNextToken :: Lens' JourneysResponse (Maybe Text)
jNextToken = lens _jNextToken (\s a -> s {_jNextToken = a})

-- | An array of responses, one for each journey that's associated with the application.
jItem :: Lens' JourneysResponse [JourneyResponse]
jItem = lens _jItem (\s a -> s {_jItem = a}) . _Coerce

instance FromJSON JourneysResponse where
  parseJSON =
    withObject
      "JourneysResponse"
      ( \x ->
          JourneysResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable JourneysResponse

instance NFData JourneysResponse
