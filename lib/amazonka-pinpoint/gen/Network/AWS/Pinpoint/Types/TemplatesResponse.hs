{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplatesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplatesResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.TemplateResponse
import Network.AWS.Prelude

-- | Provides information about all the message templates that are associated with your Amazon Pinpoint account.
--
--
--
-- /See:/ 'templatesResponse' smart constructor.
data TemplatesResponse = TemplatesResponse'
  { _tNextToken ::
      !(Maybe Text),
    _tItem :: ![TemplateResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'tItem' - An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
templatesResponse ::
  TemplatesResponse
templatesResponse =
  TemplatesResponse' {_tNextToken = Nothing, _tItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
tNextToken :: Lens' TemplatesResponse (Maybe Text)
tNextToken = lens _tNextToken (\s a -> s {_tNextToken = a})

-- | An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
tItem :: Lens' TemplatesResponse [TemplateResponse]
tItem = lens _tItem (\s a -> s {_tItem = a}) . _Coerce

instance FromJSON TemplatesResponse where
  parseJSON =
    withObject
      "TemplatesResponse"
      ( \x ->
          TemplatesResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable TemplatesResponse

instance NFData TemplatesResponse
