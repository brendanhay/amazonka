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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of application versions.
--
--
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    (
    -- * Creating a Request
      describeApplicationVersions
    , DescribeApplicationVersions
    -- * Request Lenses
    , dVersionLabels
    , dNextToken
    , dMaxRecords
    , dApplicationName

    -- * Destructuring the Response
    , describeApplicationVersionsResponse
    , DescribeApplicationVersionsResponse
    -- * Response Lenses
    , davrsApplicationVersions
    , davrsNextToken
    , davrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to describe application versions.
--
--
--
-- /See:/ 'describeApplicationVersions' smart constructor.
data DescribeApplicationVersions = DescribeApplicationVersions'
  { _dVersionLabels   :: !(Maybe [Text])
  , _dNextToken       :: !(Maybe Text)
  , _dMaxRecords      :: !(Maybe Nat)
  , _dApplicationName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionLabels' - Specify a version label to show a specific application version.
--
-- * 'dNextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
--
-- * 'dMaxRecords' - For a paginated request. Specify a maximum number of application versions to include in each response. If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
--
-- * 'dApplicationName' - Specify an application name to show only application versions for that application.
describeApplicationVersions
    :: DescribeApplicationVersions
describeApplicationVersions =
  DescribeApplicationVersions'
    { _dVersionLabels = Nothing
    , _dNextToken = Nothing
    , _dMaxRecords = Nothing
    , _dApplicationName = Nothing
    }


-- | Specify a version label to show a specific application version.
dVersionLabels :: Lens' DescribeApplicationVersions [Text]
dVersionLabels = lens _dVersionLabels (\ s a -> s{_dVersionLabels = a}) . _Default . _Coerce

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
dNextToken :: Lens' DescribeApplicationVersions (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | For a paginated request. Specify a maximum number of application versions to include in each response. If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
dMaxRecords :: Lens' DescribeApplicationVersions (Maybe Natural)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a}) . mapping _Nat

-- | Specify an application name to show only application versions for that application.
dApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
dApplicationName = lens _dApplicationName (\ s a -> s{_dApplicationName = a})

instance AWSRequest DescribeApplicationVersions where
        type Rs DescribeApplicationVersions =
             DescribeApplicationVersionsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "DescribeApplicationVersionsResult"
              (\ s h x ->
                 DescribeApplicationVersionsResponse' <$>
                   (x .@? "ApplicationVersions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeApplicationVersions where

instance NFData DescribeApplicationVersions where

instance ToHeaders DescribeApplicationVersions where
        toHeaders = const mempty

instance ToPath DescribeApplicationVersions where
        toPath = const "/"

instance ToQuery DescribeApplicationVersions where
        toQuery DescribeApplicationVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeApplicationVersions" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "VersionLabels" =:
                 toQuery (toQueryList "member" <$> _dVersionLabels),
               "NextToken" =: _dNextToken,
               "MaxRecords" =: _dMaxRecords,
               "ApplicationName" =: _dApplicationName]

-- | Result message wrapping a list of application version descriptions.
--
--
--
-- /See:/ 'describeApplicationVersionsResponse' smart constructor.
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
  { _davrsApplicationVersions :: !(Maybe [ApplicationVersionDescription])
  , _davrsNextToken           :: !(Maybe Text)
  , _davrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davrsApplicationVersions' - List of @ApplicationVersionDescription@ objects sorted in order of creation.
--
-- * 'davrsNextToken' - In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- * 'davrsResponseStatus' - -- | The response status code.
describeApplicationVersionsResponse
    :: Int -- ^ 'davrsResponseStatus'
    -> DescribeApplicationVersionsResponse
describeApplicationVersionsResponse pResponseStatus_ =
  DescribeApplicationVersionsResponse'
    { _davrsApplicationVersions = Nothing
    , _davrsNextToken = Nothing
    , _davrsResponseStatus = pResponseStatus_
    }


-- | List of @ApplicationVersionDescription@ objects sorted in order of creation.
davrsApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrsApplicationVersions = lens _davrsApplicationVersions (\ s a -> s{_davrsApplicationVersions = a}) . _Default . _Coerce

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
davrsNextToken :: Lens' DescribeApplicationVersionsResponse (Maybe Text)
davrsNextToken = lens _davrsNextToken (\ s a -> s{_davrsNextToken = a})

-- | -- | The response status code.
davrsResponseStatus :: Lens' DescribeApplicationVersionsResponse Int
davrsResponseStatus = lens _davrsResponseStatus (\ s a -> s{_davrsResponseStatus = a})

instance NFData DescribeApplicationVersionsResponse
         where
