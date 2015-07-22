{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeApplications.html>
module Network.AWS.ElasticBeanstalk.DescribeApplications
    (
    -- * Request
      DescribeApplications
    -- ** Request constructor
    , describeApplications
    -- ** Request lenses
    , darqApplicationNames

    -- * Response
    , DescribeApplicationsResponse
    -- ** Response constructor
    , describeApplicationsResponse
    -- ** Response lenses
    , darsApplications
    , darsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'describeApplications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqApplicationNames'
newtype DescribeApplications = DescribeApplications'
    { _darqApplicationNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeApplications' smart constructor.
describeApplications :: DescribeApplications
describeApplications =
    DescribeApplications'
    { _darqApplicationNames = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
darqApplicationNames :: Lens' DescribeApplications [Text]
darqApplicationNames = lens _darqApplicationNames (\ s a -> s{_darqApplicationNames = a}) . _Default;

instance AWSRequest DescribeApplications where
        type Sv DescribeApplications = ElasticBeanstalk
        type Rs DescribeApplications =
             DescribeApplicationsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeApplicationsResult"
              (\ s h x ->
                 DescribeApplicationsResponse' <$>
                   (x .@? "Applications" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeApplications where
        toHeaders = const mempty

instance ToPath DescribeApplications where
        toPath = const "/"

instance ToQuery DescribeApplications where
        toQuery DescribeApplications'{..}
          = mconcat
              ["Action" =: ("DescribeApplications" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationNames" =:
                 toQuery
                   (toQueryList "member" <$> _darqApplicationNames)]

-- | Result message containing a list of application descriptions.
--
-- /See:/ 'describeApplicationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darsApplications'
--
-- * 'darsStatus'
data DescribeApplicationsResponse = DescribeApplicationsResponse'
    { _darsApplications :: !(Maybe [ApplicationDescription])
    , _darsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeApplicationsResponse' smart constructor.
describeApplicationsResponse :: Int -> DescribeApplicationsResponse
describeApplicationsResponse pStatus =
    DescribeApplicationsResponse'
    { _darsApplications = Nothing
    , _darsStatus = pStatus
    }

-- | This parameter contains a list of ApplicationDescription.
darsApplications :: Lens' DescribeApplicationsResponse [ApplicationDescription]
darsApplications = lens _darsApplications (\ s a -> s{_darsApplications = a}) . _Default;

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeApplicationsResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
