{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironments.html>
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    (
    -- * Request
      DescribeEnvironments
    -- ** Request constructor
    , describeEnvironments
    -- ** Request lenses
    , drqEnvironmentIds
    , drqEnvironmentNames
    , drqVersionLabel
    , drqIncludedDeletedBackTo
    , drqApplicationName
    , drqIncludeDeleted

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , drsEnvironments
    , drsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'describeEnvironments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqEnvironmentIds'
--
-- * 'drqEnvironmentNames'
--
-- * 'drqVersionLabel'
--
-- * 'drqIncludedDeletedBackTo'
--
-- * 'drqApplicationName'
--
-- * 'drqIncludeDeleted'
data DescribeEnvironments = DescribeEnvironments'
    { _drqEnvironmentIds        :: !(Maybe [Text])
    , _drqEnvironmentNames      :: !(Maybe [Text])
    , _drqVersionLabel          :: !(Maybe Text)
    , _drqIncludedDeletedBackTo :: !(Maybe ISO8601)
    , _drqApplicationName       :: !(Maybe Text)
    , _drqIncludeDeleted        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironments' smart constructor.
describeEnvironments :: DescribeEnvironments
describeEnvironments =
    DescribeEnvironments'
    { _drqEnvironmentIds = Nothing
    , _drqEnvironmentNames = Nothing
    , _drqVersionLabel = Nothing
    , _drqIncludedDeletedBackTo = Nothing
    , _drqApplicationName = Nothing
    , _drqIncludeDeleted = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
drqEnvironmentIds :: Lens' DescribeEnvironments [Text]
drqEnvironmentIds = lens _drqEnvironmentIds (\ s a -> s{_drqEnvironmentIds = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
drqEnvironmentNames :: Lens' DescribeEnvironments [Text]
drqEnvironmentNames = lens _drqEnvironmentNames (\ s a -> s{_drqEnvironmentNames = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
drqVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
drqVersionLabel = lens _drqVersionLabel (\ s a -> s{_drqVersionLabel = a});

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
drqIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
drqIncludedDeletedBackTo = lens _drqIncludedDeletedBackTo (\ s a -> s{_drqIncludedDeletedBackTo = a}) . mapping _Time;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
drqApplicationName :: Lens' DescribeEnvironments (Maybe Text)
drqApplicationName = lens _drqApplicationName (\ s a -> s{_drqApplicationName = a});

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
drqIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
drqIncludeDeleted = lens _drqIncludeDeleted (\ s a -> s{_drqIncludeDeleted = a});

instance AWSRequest DescribeEnvironments where
        type Sv DescribeEnvironments = ElasticBeanstalk
        type Rs DescribeEnvironments =
             DescribeEnvironmentsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeEnvironmentsResult"
              (\ s h x ->
                 DescribeEnvironmentsResponse' <$>
                   (x .@? "Environments" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeEnvironments where
        toHeaders = const mempty

instance ToPath DescribeEnvironments where
        toPath = const "/"

instance ToQuery DescribeEnvironments where
        toQuery DescribeEnvironments'{..}
          = mconcat
              ["Action" =: ("DescribeEnvironments" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentIds" =:
                 toQuery
                   (toQueryList "member" <$> _drqEnvironmentIds),
               "EnvironmentNames" =:
                 toQuery
                   (toQueryList "member" <$> _drqEnvironmentNames),
               "VersionLabel" =: _drqVersionLabel,
               "IncludedDeletedBackTo" =: _drqIncludedDeletedBackTo,
               "ApplicationName" =: _drqApplicationName,
               "IncludeDeleted" =: _drqIncludeDeleted]

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'describeEnvironmentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsEnvironments'
--
-- * 'drsStatus'
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
    { _drsEnvironments :: !(Maybe [EnvironmentDescription])
    , _drsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentsResponse' smart constructor.
describeEnvironmentsResponse :: Int -> DescribeEnvironmentsResponse
describeEnvironmentsResponse pStatus =
    DescribeEnvironmentsResponse'
    { _drsEnvironments = Nothing
    , _drsStatus = pStatus
    }

-- | Returns an EnvironmentDescription list.
drsEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
drsEnvironments = lens _drsEnvironments (\ s a -> s{_drsEnvironments = a}) . _Default;

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeEnvironmentsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
