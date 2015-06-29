{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns descriptions for existing application versions.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeApplicationVersions.html>
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    (
    -- * Request
      DescribeApplicationVersions
    -- ** Request constructor
    , describeApplicationVersions
    -- ** Request lenses
    , dVersionLabels
    , dApplicationName

    -- * Response
    , DescribeApplicationVersionsResponse
    -- ** Response constructor
    , describeApplicationVersionsResponse
    -- ** Response lenses
    , davrApplicationVersions
    , davrStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Result message containing a list of configuration descriptions.
--
-- /See:/ 'describeApplicationVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dVersionLabels'
--
-- * 'dApplicationName'
data DescribeApplicationVersions = DescribeApplicationVersions'
    { _dVersionLabels   :: !(Maybe [Text])
    , _dApplicationName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeApplicationVersions' smart constructor.
describeApplicationVersions :: DescribeApplicationVersions
describeApplicationVersions =
    DescribeApplicationVersions'
    { _dVersionLabels = Nothing
    , _dApplicationName = Nothing
    }

-- | If specified, restricts the returned descriptions to only include ones
-- that have the specified version labels.
dVersionLabels :: Lens' DescribeApplicationVersions [Text]
dVersionLabels = lens _dVersionLabels (\ s a -> s{_dVersionLabels = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include ones that are associated with the specified application.
dApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
dApplicationName = lens _dApplicationName (\ s a -> s{_dApplicationName = a});

instance AWSRequest DescribeApplicationVersions where
        type Sv DescribeApplicationVersions =
             ElasticBeanstalk
        type Rs DescribeApplicationVersions =
             DescribeApplicationVersionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeApplicationVersionsResult"
              (\ s h x ->
                 DescribeApplicationVersionsResponse' <$>
                   (x .@? "ApplicationVersions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
               "ApplicationName" =: _dApplicationName]

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'describeApplicationVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davrApplicationVersions'
--
-- * 'davrStatus'
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
    { _davrApplicationVersions :: !(Maybe [ApplicationVersionDescription])
    , _davrStatus              :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeApplicationVersionsResponse' smart constructor.
describeApplicationVersionsResponse :: Int -> DescribeApplicationVersionsResponse
describeApplicationVersionsResponse pStatus =
    DescribeApplicationVersionsResponse'
    { _davrApplicationVersions = Nothing
    , _davrStatus = pStatus
    }

-- | A list of ApplicationVersionDescription .
davrApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrApplicationVersions = lens _davrApplicationVersions (\ s a -> s{_davrApplicationVersions = a}) . _Default;

-- | FIXME: Undocumented member.
davrStatus :: Lens' DescribeApplicationVersionsResponse Int
davrStatus = lens _davrStatus (\ s a -> s{_davrStatus = a});
