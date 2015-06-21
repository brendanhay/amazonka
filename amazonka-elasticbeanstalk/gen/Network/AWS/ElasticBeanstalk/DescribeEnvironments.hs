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
    , desVersionLabel
    , desIncludedDeletedBackTo
    , desApplicationName
    , desIncludeDeleted

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , derEnvironments
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEnvironments' smart constructor.
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
data DescribeEnvironments = DescribeEnvironments'{_desEnvironmentIds :: Maybe [Text], _desEnvironmentNames :: Maybe [Text], _desVersionLabel :: Maybe Text, _desIncludedDeletedBackTo :: Maybe ISO8601, _desApplicationName :: Maybe Text, _desIncludeDeleted :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DescribeEnvironments' smart constructor.
describeEnvironments :: DescribeEnvironments
describeEnvironments = DescribeEnvironments'{_desEnvironmentIds = Nothing, _desEnvironmentNames = Nothing, _desVersionLabel = Nothing, _desIncludedDeletedBackTo = Nothing, _desApplicationName = Nothing, _desIncludeDeleted = Nothing};

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
                      may (parseXMLList "member")))

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

-- | /See:/ 'describeEnvironmentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironments'
newtype DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'{_derEnvironments :: Maybe [EnvironmentDescription]} deriving (Eq, Read, Show)

-- | 'DescribeEnvironmentsResponse' smart constructor.
describeEnvironmentsResponse :: DescribeEnvironmentsResponse
describeEnvironmentsResponse = DescribeEnvironmentsResponse'{_derEnvironments = Nothing};

-- | Returns an EnvironmentDescription list.
derEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
derEnvironments = lens _derEnvironments (\ s a -> s{_derEnvironments = a}) . _Default;
