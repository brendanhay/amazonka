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
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available platforms.
--
--
module Network.AWS.ElasticBeanstalk.ListPlatformVersions
    (
    -- * Creating a Request
      listPlatformVersions
    , ListPlatformVersions
    -- * Request Lenses
    , lpvFilters
    , lpvNextToken
    , lpvMaxRecords

    -- * Destructuring the Response
    , listPlatformVersionsResponse
    , ListPlatformVersionsResponse
    -- * Response Lenses
    , lpvrsNextToken
    , lpvrsPlatformSummaryList
    , lpvrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { _lpvFilters    :: !(Maybe [PlatformFilter])
  , _lpvNextToken  :: !(Maybe Text)
  , _lpvMaxRecords :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPlatformVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvFilters' - List only the platforms where the platform member value relates to one of the supplied values.
--
-- * 'lpvNextToken' - The starting index into the remaining list of platforms. Use the @NextToken@ value from a previous @ListPlatformVersion@ call.
--
-- * 'lpvMaxRecords' - The maximum number of platform values returned in one call.
listPlatformVersions
    :: ListPlatformVersions
listPlatformVersions =
  ListPlatformVersions'
    {_lpvFilters = Nothing, _lpvNextToken = Nothing, _lpvMaxRecords = Nothing}


-- | List only the platforms where the platform member value relates to one of the supplied values.
lpvFilters :: Lens' ListPlatformVersions [PlatformFilter]
lpvFilters = lens _lpvFilters (\ s a -> s{_lpvFilters = a}) . _Default . _Coerce

-- | The starting index into the remaining list of platforms. Use the @NextToken@ value from a previous @ListPlatformVersion@ call.
lpvNextToken :: Lens' ListPlatformVersions (Maybe Text)
lpvNextToken = lens _lpvNextToken (\ s a -> s{_lpvNextToken = a})

-- | The maximum number of platform values returned in one call.
lpvMaxRecords :: Lens' ListPlatformVersions (Maybe Natural)
lpvMaxRecords = lens _lpvMaxRecords (\ s a -> s{_lpvMaxRecords = a}) . mapping _Nat

instance AWSRequest ListPlatformVersions where
        type Rs ListPlatformVersions =
             ListPlatformVersionsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "ListPlatformVersionsResult"
              (\ s h x ->
                 ListPlatformVersionsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "PlatformSummaryList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListPlatformVersions where

instance NFData ListPlatformVersions where

instance ToHeaders ListPlatformVersions where
        toHeaders = const mempty

instance ToPath ListPlatformVersions where
        toPath = const "/"

instance ToQuery ListPlatformVersions where
        toQuery ListPlatformVersions'{..}
          = mconcat
              ["Action" =: ("ListPlatformVersions" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "member" <$> _lpvFilters),
               "NextToken" =: _lpvNextToken,
               "MaxRecords" =: _lpvMaxRecords]

-- | /See:/ 'listPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { _lpvrsNextToken           :: !(Maybe Text)
  , _lpvrsPlatformSummaryList :: !(Maybe [PlatformSummary])
  , _lpvrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPlatformVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsNextToken' - The starting index into the remaining list of platforms. if this value is not @null@ , you can use it in a subsequent @ListPlatformVersion@ call.
--
-- * 'lpvrsPlatformSummaryList' - Detailed information about the platforms.
--
-- * 'lpvrsResponseStatus' - -- | The response status code.
listPlatformVersionsResponse
    :: Int -- ^ 'lpvrsResponseStatus'
    -> ListPlatformVersionsResponse
listPlatformVersionsResponse pResponseStatus_ =
  ListPlatformVersionsResponse'
    { _lpvrsNextToken = Nothing
    , _lpvrsPlatformSummaryList = Nothing
    , _lpvrsResponseStatus = pResponseStatus_
    }


-- | The starting index into the remaining list of platforms. if this value is not @null@ , you can use it in a subsequent @ListPlatformVersion@ call.
lpvrsNextToken :: Lens' ListPlatformVersionsResponse (Maybe Text)
lpvrsNextToken = lens _lpvrsNextToken (\ s a -> s{_lpvrsNextToken = a})

-- | Detailed information about the platforms.
lpvrsPlatformSummaryList :: Lens' ListPlatformVersionsResponse [PlatformSummary]
lpvrsPlatformSummaryList = lens _lpvrsPlatformSummaryList (\ s a -> s{_lpvrsPlatformSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lpvrsResponseStatus :: Lens' ListPlatformVersionsResponse Int
lpvrsResponseStatus = lens _lpvrsResponseStatus (\ s a -> s{_lpvrsResponseStatus = a})

instance NFData ListPlatformVersionsResponse where
