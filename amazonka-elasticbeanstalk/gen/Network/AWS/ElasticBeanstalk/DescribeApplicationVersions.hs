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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of application versions stored in your AWS Elastic Beanstalk storage bucket.
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

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Result message containing a list of configuration descriptions.
--
--
--
-- /See:/ 'describeApplicationVersions' smart constructor.
data DescribeApplicationVersions = DescribeApplicationVersions'
    { _dVersionLabels   :: !(Maybe [Text])
    , _dNextToken       :: !(Maybe Text)
    , _dMaxRecords      :: !(Maybe Nat)
    , _dApplicationName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeApplicationVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionLabels' - If specified, restricts the returned descriptions to only include ones that have the specified version labels.
--
-- * 'dNextToken' - Specify a next token to retrieve the next page in a paginated request.
--
-- * 'dMaxRecords' - Specify a maximum number of application versions to paginate in the request.
--
-- * 'dApplicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include ones that are associated with the specified application.
describeApplicationVersions
    :: DescribeApplicationVersions
describeApplicationVersions =
    DescribeApplicationVersions'
    { _dVersionLabels = Nothing
    , _dNextToken = Nothing
    , _dMaxRecords = Nothing
    , _dApplicationName = Nothing
    }

-- | If specified, restricts the returned descriptions to only include ones that have the specified version labels.
dVersionLabels :: Lens' DescribeApplicationVersions [Text]
dVersionLabels = lens _dVersionLabels (\ s a -> s{_dVersionLabels = a}) . _Default . _Coerce;

-- | Specify a next token to retrieve the next page in a paginated request.
dNextToken :: Lens' DescribeApplicationVersions (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | Specify a maximum number of application versions to paginate in the request.
dMaxRecords :: Lens' DescribeApplicationVersions (Maybe Natural)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a}) . mapping _Nat;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include ones that are associated with the specified application.
dApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
dApplicationName = lens _dApplicationName (\ s a -> s{_dApplicationName = a});

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

instance Hashable DescribeApplicationVersions

instance NFData DescribeApplicationVersions

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeApplicationVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davrsApplicationVersions' - List of @ApplicationVersionDescription@ objects sorted by order of creation.
--
-- * 'davrsNextToken' - For a paginated request, the token that you can pass in a subsequent request to get the next page.
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

-- | List of @ApplicationVersionDescription@ objects sorted by order of creation.
davrsApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrsApplicationVersions = lens _davrsApplicationVersions (\ s a -> s{_davrsApplicationVersions = a}) . _Default . _Coerce;

-- | For a paginated request, the token that you can pass in a subsequent request to get the next page.
davrsNextToken :: Lens' DescribeApplicationVersionsResponse (Maybe Text)
davrsNextToken = lens _davrsNextToken (\ s a -> s{_davrsNextToken = a});

-- | -- | The response status code.
davrsResponseStatus :: Lens' DescribeApplicationVersionsResponse Int
davrsResponseStatus = lens _davrsResponseStatus (\ s a -> s{_davrsResponseStatus = a});

instance NFData DescribeApplicationVersionsResponse
