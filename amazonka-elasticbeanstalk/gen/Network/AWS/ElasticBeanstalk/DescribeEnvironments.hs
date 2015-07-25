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
    , dEnvironmentIds
    , dEnvironmentNames
    , dVersionLabel
    , dIncludedDeletedBackTo
    , dApplicationName
    , dIncludeDeleted

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
-- * 'dEnvironmentIds'
--
-- * 'dEnvironmentNames'
--
-- * 'dVersionLabel'
--
-- * 'dIncludedDeletedBackTo'
--
-- * 'dApplicationName'
--
-- * 'dIncludeDeleted'
data DescribeEnvironments = DescribeEnvironments'
    { _dEnvironmentIds        :: !(Maybe [Text])
    , _dEnvironmentNames      :: !(Maybe [Text])
    , _dVersionLabel          :: !(Maybe Text)
    , _dIncludedDeletedBackTo :: !(Maybe ISO8601)
    , _dApplicationName       :: !(Maybe Text)
    , _dIncludeDeleted        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEnvironments' smart constructor.
describeEnvironments :: DescribeEnvironments
describeEnvironments =
    DescribeEnvironments'
    { _dEnvironmentIds = Nothing
    , _dEnvironmentNames = Nothing
    , _dVersionLabel = Nothing
    , _dIncludedDeletedBackTo = Nothing
    , _dApplicationName = Nothing
    , _dIncludeDeleted = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
dEnvironmentIds :: Lens' DescribeEnvironments [Text]
dEnvironmentIds = lens _dEnvironmentIds (\ s a -> s{_dEnvironmentIds = a}) . _Default . _Coerce;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
dEnvironmentNames :: Lens' DescribeEnvironments [Text]
dEnvironmentNames = lens _dEnvironmentNames (\ s a -> s{_dEnvironmentNames = a}) . _Default . _Coerce;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
dVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
dVersionLabel = lens _dVersionLabel (\ s a -> s{_dVersionLabel = a});

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
dIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
dIncludedDeletedBackTo = lens _dIncludedDeletedBackTo (\ s a -> s{_dIncludedDeletedBackTo = a}) . mapping _Time;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
dApplicationName :: Lens' DescribeEnvironments (Maybe Text)
dApplicationName = lens _dApplicationName (\ s a -> s{_dApplicationName = a});

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
dIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
dIncludeDeleted = lens _dIncludeDeleted (\ s a -> s{_dIncludeDeleted = a});

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
                 toQuery (toQueryList "member" <$> _dEnvironmentIds),
               "EnvironmentNames" =:
                 toQuery
                   (toQueryList "member" <$> _dEnvironmentNames),
               "VersionLabel" =: _dVersionLabel,
               "IncludedDeletedBackTo" =: _dIncludedDeletedBackTo,
               "ApplicationName" =: _dApplicationName,
               "IncludeDeleted" =: _dIncludeDeleted]

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
describeEnvironmentsResponse pStatus_ =
    DescribeEnvironmentsResponse'
    { _drsEnvironments = Nothing
    , _drsStatus = pStatus_
    }

-- | Returns an EnvironmentDescription list.
drsEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
drsEnvironments = lens _drsEnvironments (\ s a -> s{_drsEnvironments = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeEnvironmentsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
