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
-- Module      : Network.AWS.AppStream.DescribeDirectoryConfigs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
--
module Network.AWS.AppStream.DescribeDirectoryConfigs
    (
    -- * Creating a Request
      describeDirectoryConfigs
    , DescribeDirectoryConfigs
    -- * Request Lenses
    , ddcNextToken
    , ddcDirectoryNames
    , ddcMaxResults

    -- * Destructuring the Response
    , describeDirectoryConfigsResponse
    , DescribeDirectoryConfigsResponse
    -- * Response Lenses
    , ddcrsNextToken
    , ddcrsDirectoryConfigs
    , ddcrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDirectoryConfigs' smart constructor.
data DescribeDirectoryConfigs = DescribeDirectoryConfigs'
  { _ddcNextToken      :: !(Maybe Text)
  , _ddcDirectoryNames :: !(Maybe [Text])
  , _ddcMaxResults     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectoryConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'ddcDirectoryNames' - The directory names.
--
-- * 'ddcMaxResults' - The maximum size of each page of results.
describeDirectoryConfigs
    :: DescribeDirectoryConfigs
describeDirectoryConfigs =
  DescribeDirectoryConfigs'
    { _ddcNextToken = Nothing
    , _ddcDirectoryNames = Nothing
    , _ddcMaxResults = Nothing
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
ddcNextToken :: Lens' DescribeDirectoryConfigs (Maybe Text)
ddcNextToken = lens _ddcNextToken (\ s a -> s{_ddcNextToken = a})

-- | The directory names.
ddcDirectoryNames :: Lens' DescribeDirectoryConfigs [Text]
ddcDirectoryNames = lens _ddcDirectoryNames (\ s a -> s{_ddcDirectoryNames = a}) . _Default . _Coerce

-- | The maximum size of each page of results.
ddcMaxResults :: Lens' DescribeDirectoryConfigs (Maybe Int)
ddcMaxResults = lens _ddcMaxResults (\ s a -> s{_ddcMaxResults = a})

instance AWSRequest DescribeDirectoryConfigs where
        type Rs DescribeDirectoryConfigs =
             DescribeDirectoryConfigsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectoryConfigsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "DirectoryConfigs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDirectoryConfigs where

instance NFData DescribeDirectoryConfigs where

instance ToHeaders DescribeDirectoryConfigs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeDirectoryConfigs"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDirectoryConfigs where
        toJSON DescribeDirectoryConfigs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ddcNextToken,
                  ("DirectoryNames" .=) <$> _ddcDirectoryNames,
                  ("MaxResults" .=) <$> _ddcMaxResults])

instance ToPath DescribeDirectoryConfigs where
        toPath = const "/"

instance ToQuery DescribeDirectoryConfigs where
        toQuery = const mempty

-- | /See:/ 'describeDirectoryConfigsResponse' smart constructor.
data DescribeDirectoryConfigsResponse = DescribeDirectoryConfigsResponse'
  { _ddcrsNextToken        :: !(Maybe Text)
  , _ddcrsDirectoryConfigs :: !(Maybe [DirectoryConfig])
  , _ddcrsResponseStatus   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectoryConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'ddcrsDirectoryConfigs' - Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
-- * 'ddcrsResponseStatus' - -- | The response status code.
describeDirectoryConfigsResponse
    :: Int -- ^ 'ddcrsResponseStatus'
    -> DescribeDirectoryConfigsResponse
describeDirectoryConfigsResponse pResponseStatus_ =
  DescribeDirectoryConfigsResponse'
    { _ddcrsNextToken = Nothing
    , _ddcrsDirectoryConfigs = Nothing
    , _ddcrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
ddcrsNextToken :: Lens' DescribeDirectoryConfigsResponse (Maybe Text)
ddcrsNextToken = lens _ddcrsNextToken (\ s a -> s{_ddcrsNextToken = a})

-- | Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
ddcrsDirectoryConfigs :: Lens' DescribeDirectoryConfigsResponse [DirectoryConfig]
ddcrsDirectoryConfigs = lens _ddcrsDirectoryConfigs (\ s a -> s{_ddcrsDirectoryConfigs = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcrsResponseStatus :: Lens' DescribeDirectoryConfigsResponse Int
ddcrsResponseStatus = lens _ddcrsResponseStatus (\ s a -> s{_ddcrsResponseStatus = a})

instance NFData DescribeDirectoryConfigsResponse
         where
