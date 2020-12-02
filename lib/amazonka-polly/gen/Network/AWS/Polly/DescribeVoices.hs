{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.DescribeVoices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name.
--
--
-- When synthesizing speech ( @SynthesizeSpeech@ ), you provide the voice ID for the voice you want from the list of voices returned by @DescribeVoices@ .
--
-- For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the @DescribeVoices@ operation you can provide the user with a list of available voices to select from.
--
-- You can optionally specify a language code to filter the available voices. For example, if you specify @en-US@ , the operation returns a list of all available US English voices.
--
-- This operation requires permissions to perform the @polly:DescribeVoices@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Polly.DescribeVoices
    (
    -- * Creating a Request
      describeVoices
    , DescribeVoices
    -- * Request Lenses
    , dvLanguageCode
    , dvNextToken

    -- * Destructuring the Response
    , describeVoicesResponse
    , DescribeVoicesResponse
    -- * Response Lenses
    , dvrsNextToken
    , dvrsVoices
    , dvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVoices' smart constructor.
data DescribeVoices = DescribeVoices'
  { _dvLanguageCode :: !(Maybe LanguageCode)
  , _dvNextToken    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVoices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvLanguageCode' - The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned.
--
-- * 'dvNextToken' - An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
describeVoices
    :: DescribeVoices
describeVoices =
  DescribeVoices' {_dvLanguageCode = Nothing, _dvNextToken = Nothing}


-- | The language identification tag (ISO 639 code for the language name-ISO 3166 country code) for filtering the list of voices returned. If you don't specify this optional parameter, all available voices are returned.
dvLanguageCode :: Lens' DescribeVoices (Maybe LanguageCode)
dvLanguageCode = lens _dvLanguageCode (\ s a -> s{_dvLanguageCode = a})

-- | An opaque pagination token returned from the previous @DescribeVoices@ operation. If present, this indicates where to continue the listing.
dvNextToken :: Lens' DescribeVoices (Maybe Text)
dvNextToken = lens _dvNextToken (\ s a -> s{_dvNextToken = a})

instance AWSPager DescribeVoices where
        page rq rs
          | stop (rs ^. dvrsNextToken) = Nothing
          | stop (rs ^. dvrsVoices) = Nothing
          | otherwise =
            Just $ rq & dvNextToken .~ rs ^. dvrsNextToken

instance AWSRequest DescribeVoices where
        type Rs DescribeVoices = DescribeVoicesResponse
        request = get polly
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVoicesResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Voices" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVoices where

instance NFData DescribeVoices where

instance ToHeaders DescribeVoices where
        toHeaders = const mempty

instance ToPath DescribeVoices where
        toPath = const "/v1/voices"

instance ToQuery DescribeVoices where
        toQuery DescribeVoices'{..}
          = mconcat
              ["LanguageCode" =: _dvLanguageCode,
               "NextToken" =: _dvNextToken]

-- | /See:/ 'describeVoicesResponse' smart constructor.
data DescribeVoicesResponse = DescribeVoicesResponse'
  { _dvrsNextToken      :: !(Maybe Text)
  , _dvrsVoices         :: !(Maybe [Voice])
  , _dvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVoicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrsNextToken' - The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
--
-- * 'dvrsVoices' - A list of voices with their properties.
--
-- * 'dvrsResponseStatus' - -- | The response status code.
describeVoicesResponse
    :: Int -- ^ 'dvrsResponseStatus'
    -> DescribeVoicesResponse
describeVoicesResponse pResponseStatus_ =
  DescribeVoicesResponse'
    { _dvrsNextToken = Nothing
    , _dvrsVoices = Nothing
    , _dvrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use in the next request to continue the listing of voices. @NextToken@ is returned only if the response is truncated.
dvrsNextToken :: Lens' DescribeVoicesResponse (Maybe Text)
dvrsNextToken = lens _dvrsNextToken (\ s a -> s{_dvrsNextToken = a})

-- | A list of voices with their properties.
dvrsVoices :: Lens' DescribeVoicesResponse [Voice]
dvrsVoices = lens _dvrsVoices (\ s a -> s{_dvrsVoices = a}) . _Default . _Coerce

-- | -- | The response status code.
dvrsResponseStatus :: Lens' DescribeVoicesResponse Int
dvrsResponseStatus = lens _dvrsResponseStatus (\ s a -> s{_dvrsResponseStatus = a})

instance NFData DescribeVoicesResponse where
