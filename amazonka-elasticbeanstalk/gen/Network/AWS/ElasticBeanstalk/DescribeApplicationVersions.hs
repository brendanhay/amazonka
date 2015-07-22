{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing application versions.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeApplicationVersions.html>
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    (
    -- * Request
      DescribeApplicationVersions
    -- ** Request constructor
    , describeApplicationVersions
    -- ** Request lenses
    , davsrqVersionLabels
    , davsrqApplicationName

    -- * Response
    , DescribeApplicationVersionsResponse
    -- ** Response constructor
    , describeApplicationVersionsResponse
    -- ** Response lenses
    , davrsApplicationVersions
    , davrsStatus
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
-- * 'davsrqVersionLabels'
--
-- * 'davsrqApplicationName'
data DescribeApplicationVersions = DescribeApplicationVersions'
    { _davsrqVersionLabels   :: !(Maybe [Text])
    , _davsrqApplicationName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeApplicationVersions' smart constructor.
describeApplicationVersions :: DescribeApplicationVersions
describeApplicationVersions =
    DescribeApplicationVersions'
    { _davsrqVersionLabels = Nothing
    , _davsrqApplicationName = Nothing
    }

-- | If specified, restricts the returned descriptions to only include ones
-- that have the specified version labels.
davsrqVersionLabels :: Lens' DescribeApplicationVersions [Text]
davsrqVersionLabels = lens _davsrqVersionLabels (\ s a -> s{_davsrqVersionLabels = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include ones that are associated with the specified application.
davsrqApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
davsrqApplicationName = lens _davsrqApplicationName (\ s a -> s{_davsrqApplicationName = a});

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
                 toQuery
                   (toQueryList "member" <$> _davsrqVersionLabels),
               "ApplicationName" =: _davsrqApplicationName]

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'describeApplicationVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davrsApplicationVersions'
--
-- * 'davrsStatus'
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
    { _davrsApplicationVersions :: !(Maybe [ApplicationVersionDescription])
    , _davrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeApplicationVersionsResponse' smart constructor.
describeApplicationVersionsResponse :: Int -> DescribeApplicationVersionsResponse
describeApplicationVersionsResponse pStatus =
    DescribeApplicationVersionsResponse'
    { _davrsApplicationVersions = Nothing
    , _davrsStatus = pStatus
    }

-- | A list of ApplicationVersionDescription .
davrsApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrsApplicationVersions = lens _davrsApplicationVersions (\ s a -> s{_davrsApplicationVersions = a}) . _Default;

-- | FIXME: Undocumented member.
davrsStatus :: Lens' DescribeApplicationVersionsResponse Int
davrsStatus = lens _davrsStatus (\ s a -> s{_davrsStatus = a});
