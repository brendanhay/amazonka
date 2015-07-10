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
    , desEnvironmentIds
    , desEnvironmentNames
    , desVersionLabel
    , desIncludedDeletedBackTo
    , desApplicationName
    , desIncludeDeleted

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , desEnvironments
    , desStatus
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
-- * 'desEnvironmentIds'
--
-- * 'desEnvironmentNames'
--
-- * 'desVersionLabel'
--
-- * 'desIncludedDeletedBackTo'
--
-- * 'desApplicationName'
--
-- * 'desIncludeDeleted'
data DescribeEnvironments = DescribeEnvironments'
    { _desEnvironmentIds        :: !(Maybe [Text])
    , _desEnvironmentNames      :: !(Maybe [Text])
    , _desVersionLabel          :: !(Maybe Text)
    , _desIncludedDeletedBackTo :: !(Maybe ISO8601)
    , _desApplicationName       :: !(Maybe Text)
    , _desIncludeDeleted        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironments' smart constructor.
describeEnvironments :: DescribeEnvironments
describeEnvironments =
    DescribeEnvironments'
    { _desEnvironmentIds = Nothing
    , _desEnvironmentNames = Nothing
    , _desVersionLabel = Nothing
    , _desIncludedDeletedBackTo = Nothing
    , _desApplicationName = Nothing
    , _desIncludeDeleted = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
desEnvironmentIds :: Lens' DescribeEnvironments [Text]
desEnvironmentIds = lens _desEnvironmentIds (\ s a -> s{_desEnvironmentIds = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
desEnvironmentNames :: Lens' DescribeEnvironments [Text]
desEnvironmentNames = lens _desEnvironmentNames (\ s a -> s{_desEnvironmentNames = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
desVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
desVersionLabel = lens _desVersionLabel (\ s a -> s{_desVersionLabel = a});

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
desIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
desIncludedDeletedBackTo = lens _desIncludedDeletedBackTo (\ s a -> s{_desIncludedDeletedBackTo = a}) . mapping _Time;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
desApplicationName :: Lens' DescribeEnvironments (Maybe Text)
desApplicationName = lens _desApplicationName (\ s a -> s{_desApplicationName = a});

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
desIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
desIncludeDeleted = lens _desIncludeDeleted (\ s a -> s{_desIncludeDeleted = a});

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
                   (toQueryList "member" <$> _desEnvironmentIds),
               "EnvironmentNames" =:
                 toQuery
                   (toQueryList "member" <$> _desEnvironmentNames),
               "VersionLabel" =: _desVersionLabel,
               "IncludedDeletedBackTo" =: _desIncludedDeletedBackTo,
               "ApplicationName" =: _desApplicationName,
               "IncludeDeleted" =: _desIncludeDeleted]

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'describeEnvironmentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desEnvironments'
--
-- * 'desStatus'
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
    { _desEnvironments :: !(Maybe [EnvironmentDescription])
    , _desStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironmentsResponse' smart constructor.
describeEnvironmentsResponse :: Int -> DescribeEnvironmentsResponse
describeEnvironmentsResponse pStatus =
    DescribeEnvironmentsResponse'
    { _desEnvironments = Nothing
    , _desStatus = pStatus
    }

-- | Returns an EnvironmentDescription list.
desEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
desEnvironments = lens _desEnvironments (\ s a -> s{_desEnvironments = a}) . _Default;

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeEnvironmentsResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
