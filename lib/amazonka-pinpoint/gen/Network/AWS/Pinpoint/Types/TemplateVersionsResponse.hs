{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateVersionsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateVersionResponse
import Network.AWS.Prelude

-- | Provides information about all the versions of a specific message template.
--
--
--
-- /See:/ 'templateVersionsResponse' smart constructor.
data TemplateVersionsResponse = TemplateVersionsResponse'
  { _tvRequestId ::
      !(Maybe Text),
    _tvNextToken :: !(Maybe Text),
    _tvMessage :: !(Maybe Text),
    _tvItem :: ![TemplateVersionResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplateVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvRequestId' - The unique identifier for the request to retrieve information about all the versions of the message template.
--
-- * 'tvNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'tvMessage' - The message that's returned from the API for the request to retrieve information about all the versions of the message template.
--
-- * 'tvItem' - An array of responses, one for each version of the message template.
templateVersionsResponse ::
  TemplateVersionsResponse
templateVersionsResponse =
  TemplateVersionsResponse'
    { _tvRequestId = Nothing,
      _tvNextToken = Nothing,
      _tvMessage = Nothing,
      _tvItem = mempty
    }

-- | The unique identifier for the request to retrieve information about all the versions of the message template.
tvRequestId :: Lens' TemplateVersionsResponse (Maybe Text)
tvRequestId = lens _tvRequestId (\s a -> s {_tvRequestId = a})

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
tvNextToken :: Lens' TemplateVersionsResponse (Maybe Text)
tvNextToken = lens _tvNextToken (\s a -> s {_tvNextToken = a})

-- | The message that's returned from the API for the request to retrieve information about all the versions of the message template.
tvMessage :: Lens' TemplateVersionsResponse (Maybe Text)
tvMessage = lens _tvMessage (\s a -> s {_tvMessage = a})

-- | An array of responses, one for each version of the message template.
tvItem :: Lens' TemplateVersionsResponse [TemplateVersionResponse]
tvItem = lens _tvItem (\s a -> s {_tvItem = a}) . _Coerce

instance FromJSON TemplateVersionsResponse where
  parseJSON =
    withObject
      "TemplateVersionsResponse"
      ( \x ->
          TemplateVersionsResponse'
            <$> (x .:? "RequestID")
            <*> (x .:? "NextToken")
            <*> (x .:? "Message")
            <*> (x .:? "Item" .!= mempty)
      )

instance Hashable TemplateVersionsResponse

instance NFData TemplateVersionsResponse
