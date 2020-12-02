{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform versions available for your account in an AWS Region. Provides summary information about each platform version. Compare to 'DescribePlatformVersion' , which provides full details about a single platform version.
--
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.ListPlatformVersions
  ( -- * Creating a Request
    listPlatformVersions,
    ListPlatformVersions,

    -- * Request Lenses
    lpvFilters,
    lpvNextToken,
    lpvMaxRecords,

    -- * Destructuring the Response
    listPlatformVersionsResponse,
    ListPlatformVersionsResponse,

    -- * Response Lenses
    lpvrsNextToken,
    lpvrsPlatformSummaryList,
    lpvrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { _lpvFilters ::
      !(Maybe [PlatformFilter]),
    _lpvNextToken :: !(Maybe Text),
    _lpvMaxRecords :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPlatformVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvFilters' - Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
--
-- * 'lpvNextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
--
-- * 'lpvMaxRecords' - The maximum number of platform version values returned in one call.
listPlatformVersions ::
  ListPlatformVersions
listPlatformVersions =
  ListPlatformVersions'
    { _lpvFilters = Nothing,
      _lpvNextToken = Nothing,
      _lpvMaxRecords = Nothing
    }

-- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
lpvFilters :: Lens' ListPlatformVersions [PlatformFilter]
lpvFilters = lens _lpvFilters (\s a -> s {_lpvFilters = a}) . _Default . _Coerce

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
lpvNextToken :: Lens' ListPlatformVersions (Maybe Text)
lpvNextToken = lens _lpvNextToken (\s a -> s {_lpvNextToken = a})

-- | The maximum number of platform version values returned in one call.
lpvMaxRecords :: Lens' ListPlatformVersions (Maybe Natural)
lpvMaxRecords = lens _lpvMaxRecords (\s a -> s {_lpvMaxRecords = a}) . mapping _Nat

instance AWSPager ListPlatformVersions where
  page rq rs
    | stop (rs ^. lpvrsNextToken) = Nothing
    | stop (rs ^. lpvrsPlatformSummaryList) = Nothing
    | otherwise = Just $ rq & lpvNextToken .~ rs ^. lpvrsNextToken

instance AWSRequest ListPlatformVersions where
  type Rs ListPlatformVersions = ListPlatformVersionsResponse
  request = postQuery elasticBeanstalk
  response =
    receiveXMLWrapper
      "ListPlatformVersionsResult"
      ( \s h x ->
          ListPlatformVersionsResponse'
            <$> (x .@? "NextToken")
            <*> ( x .@? "PlatformSummaryList" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable ListPlatformVersions

instance NFData ListPlatformVersions

instance ToHeaders ListPlatformVersions where
  toHeaders = const mempty

instance ToPath ListPlatformVersions where
  toPath = const "/"

instance ToQuery ListPlatformVersions where
  toQuery ListPlatformVersions' {..} =
    mconcat
      [ "Action" =: ("ListPlatformVersions" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "Filters" =: toQuery (toQueryList "member" <$> _lpvFilters),
        "NextToken" =: _lpvNextToken,
        "MaxRecords" =: _lpvMaxRecords
      ]

-- | /See:/ 'listPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { _lpvrsNextToken ::
      !(Maybe Text),
    _lpvrsPlatformSummaryList ::
      !(Maybe [PlatformSummary]),
    _lpvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPlatformVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsNextToken' - In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- * 'lpvrsPlatformSummaryList' - Summary information about the platform versions.
--
-- * 'lpvrsResponseStatus' - -- | The response status code.
listPlatformVersionsResponse ::
  -- | 'lpvrsResponseStatus'
  Int ->
  ListPlatformVersionsResponse
listPlatformVersionsResponse pResponseStatus_ =
  ListPlatformVersionsResponse'
    { _lpvrsNextToken = Nothing,
      _lpvrsPlatformSummaryList = Nothing,
      _lpvrsResponseStatus = pResponseStatus_
    }

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
lpvrsNextToken :: Lens' ListPlatformVersionsResponse (Maybe Text)
lpvrsNextToken = lens _lpvrsNextToken (\s a -> s {_lpvrsNextToken = a})

-- | Summary information about the platform versions.
lpvrsPlatformSummaryList :: Lens' ListPlatformVersionsResponse [PlatformSummary]
lpvrsPlatformSummaryList = lens _lpvrsPlatformSummaryList (\s a -> s {_lpvrsPlatformSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lpvrsResponseStatus :: Lens' ListPlatformVersionsResponse Int
lpvrsResponseStatus = lens _lpvrsResponseStatus (\s a -> s {_lpvrsResponseStatus = a})

instance NFData ListPlatformVersionsResponse
