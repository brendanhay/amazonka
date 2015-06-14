{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
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

-- | Returns descriptions for existing environments.
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
    , desIncludedDeletedBackTo
    , desIncludeDeleted
    , desVersionLabel
    , desApplicationName

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , derEnvironments
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'describeEnvironments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desEnvironmentIds'
--
-- * 'desEnvironmentNames'
--
-- * 'desIncludedDeletedBackTo'
--
-- * 'desIncludeDeleted'
--
-- * 'desVersionLabel'
--
-- * 'desApplicationName'
data DescribeEnvironments = DescribeEnvironments'{_desEnvironmentIds :: [Text], _desEnvironmentNames :: [Text], _desIncludedDeletedBackTo :: Maybe ISO8601, _desIncludeDeleted :: Maybe Bool, _desVersionLabel :: Text, _desApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeEnvironments' smart constructor.
describeEnvironments :: Text -> Text -> DescribeEnvironments
describeEnvironments pVersionLabel pApplicationName = DescribeEnvironments'{_desEnvironmentIds = mempty, _desEnvironmentNames = mempty, _desIncludedDeletedBackTo = Nothing, _desIncludeDeleted = Nothing, _desVersionLabel = pVersionLabel, _desApplicationName = pApplicationName};

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
desEnvironmentIds :: Lens' DescribeEnvironments [Text]
desEnvironmentIds = lens _desEnvironmentIds (\ s a -> s{_desEnvironmentIds = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
desEnvironmentNames :: Lens' DescribeEnvironments [Text]
desEnvironmentNames = lens _desEnvironmentNames (\ s a -> s{_desEnvironmentNames = a});

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
desIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
desIncludedDeletedBackTo = lens _desIncludedDeletedBackTo (\ s a -> s{_desIncludedDeletedBackTo = a}) . mapping _Time;

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
desIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
desIncludeDeleted = lens _desIncludeDeleted (\ s a -> s{_desIncludeDeleted = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
desVersionLabel :: Lens' DescribeEnvironments Text
desVersionLabel = lens _desVersionLabel (\ s a -> s{_desVersionLabel = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
desApplicationName :: Lens' DescribeEnvironments Text
desApplicationName = lens _desApplicationName (\ s a -> s{_desApplicationName = a});

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
                      parseXMLList "member"))

instance ToHeaders DescribeEnvironments where
        toHeaders = const mempty

instance ToPath DescribeEnvironments where
        toPath = const "/"

instance ToQuery DescribeEnvironments where
        toQuery DescribeEnvironments'{..}
          = mconcat
              ["Action" =: ("DescribeEnvironments" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentIds" =: "member" =: _desEnvironmentIds,
               "EnvironmentNames" =:
                 "member" =: _desEnvironmentNames,
               "IncludedDeletedBackTo" =: _desIncludedDeletedBackTo,
               "IncludeDeleted" =: _desIncludeDeleted,
               "VersionLabel" =: _desVersionLabel,
               "ApplicationName" =: _desApplicationName]

-- | /See:/ 'describeEnvironmentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironments'
newtype DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'{_derEnvironments :: [EnvironmentDescription]} deriving (Eq, Read, Show)

-- | 'DescribeEnvironmentsResponse' smart constructor.
describeEnvironmentsResponse :: DescribeEnvironmentsResponse
describeEnvironmentsResponse = DescribeEnvironmentsResponse'{_derEnvironments = mempty};

-- | Returns an EnvironmentDescription list.
derEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
derEnvironments = lens _derEnvironments (\ s a -> s{_derEnvironments = a});
