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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
--
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    (
    -- * Creating a Request
      describeEnvironments
    , DescribeEnvironments
    -- * Request Lenses
    , desEnvironmentIds
    , desEnvironmentNames
    , desNextToken
    , desVersionLabel
    , desMaxRecords
    , desApplicationName
    , desIncludedDeletedBackTo
    , desIncludeDeleted

    -- * Destructuring the Response
    , environmentDescriptionsMessage
    , EnvironmentDescriptionsMessage
    -- * Response Lenses
    , edmNextToken
    , edmEnvironments
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to describe one or more environments.
--
--
--
-- /See:/ 'describeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { _desEnvironmentIds        :: !(Maybe [Text])
  , _desEnvironmentNames      :: !(Maybe [Text])
  , _desNextToken             :: !(Maybe Text)
  , _desVersionLabel          :: !(Maybe Text)
  , _desMaxRecords            :: !(Maybe Nat)
  , _desApplicationName       :: !(Maybe Text)
  , _desIncludedDeletedBackTo :: !(Maybe ISO8601)
  , _desIncludeDeleted        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desEnvironmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
--
-- * 'desEnvironmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
--
-- * 'desNextToken' - For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
--
-- * 'desVersionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
--
-- * 'desMaxRecords' - For a paginated request. Specify a maximum number of environments to include in each response. If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
--
-- * 'desApplicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
--
-- * 'desIncludedDeletedBackTo' - If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
--
-- * 'desIncludeDeleted' - Indicates whether to include deleted environments: @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed. @false@ : Do not include deleted environments.
describeEnvironments
    :: DescribeEnvironments
describeEnvironments =
  DescribeEnvironments'
    { _desEnvironmentIds = Nothing
    , _desEnvironmentNames = Nothing
    , _desNextToken = Nothing
    , _desVersionLabel = Nothing
    , _desMaxRecords = Nothing
    , _desApplicationName = Nothing
    , _desIncludedDeletedBackTo = Nothing
    , _desIncludeDeleted = Nothing
    }


-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified IDs.
desEnvironmentIds :: Lens' DescribeEnvironments [Text]
desEnvironmentIds = lens _desEnvironmentIds (\ s a -> s{_desEnvironmentIds = a}) . _Default . _Coerce

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that have the specified names.
desEnvironmentNames :: Lens' DescribeEnvironments [Text]
desEnvironmentNames = lens _desEnvironmentNames (\ s a -> s{_desEnvironmentNames = a}) . _Default . _Coerce

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request. If no @NextToken@ is specified, the first page is retrieved.
desNextToken :: Lens' DescribeEnvironments (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application version.
desVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
desVersionLabel = lens _desVersionLabel (\ s a -> s{_desVersionLabel = a})

-- | For a paginated request. Specify a maximum number of environments to include in each response. If no @MaxRecords@ is specified, all available environments are retrieved in a single response.
desMaxRecords :: Lens' DescribeEnvironments (Maybe Natural)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a}) . mapping _Nat

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those that are associated with this application.
desApplicationName :: Lens' DescribeEnvironments (Maybe Text)
desApplicationName = lens _desApplicationName (\ s a -> s{_desApplicationName = a})

-- | If specified when @IncludeDeleted@ is set to @true@ , then environments deleted after this date are displayed.
desIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
desIncludedDeletedBackTo = lens _desIncludedDeletedBackTo (\ s a -> s{_desIncludedDeletedBackTo = a}) . mapping _Time

-- | Indicates whether to include deleted environments: @true@ : Environments that have been deleted after @IncludedDeletedBackTo@ are displayed. @false@ : Do not include deleted environments.
desIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
desIncludeDeleted = lens _desIncludeDeleted (\ s a -> s{_desIncludeDeleted = a})

instance AWSRequest DescribeEnvironments where
        type Rs DescribeEnvironments =
             EnvironmentDescriptionsMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeEnvironmentsResult"
              (\ s h x -> parseXML x)

instance Hashable DescribeEnvironments where

instance NFData DescribeEnvironments where

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
               "NextToken" =: _desNextToken,
               "VersionLabel" =: _desVersionLabel,
               "MaxRecords" =: _desMaxRecords,
               "ApplicationName" =: _desApplicationName,
               "IncludedDeletedBackTo" =: _desIncludedDeletedBackTo,
               "IncludeDeleted" =: _desIncludeDeleted]
