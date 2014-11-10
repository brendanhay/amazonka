{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions for existing environments.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    (
    -- * Request
      DescribeEnvironmentsMessage
    -- ** Request constructor
    , describeEnvironments
    -- ** Request lenses
    , demApplicationName
    , demEnvironmentIds
    , demEnvironmentNames
    , demIncludeDeleted
    , demIncludedDeletedBackTo
    , demVersionLabel

    -- * Response
    , EnvironmentDescriptionsMessage
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , edmEnvironments
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeEnvironmentsMessage = DescribeEnvironmentsMessage
    { _demApplicationName       :: Maybe Text
    , _demEnvironmentIds        :: [Text]
    , _demEnvironmentNames      :: [Text]
    , _demIncludeDeleted        :: Maybe Bool
    , _demIncludedDeletedBackTo :: Maybe RFC822
    , _demVersionLabel          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEnvironmentsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'demApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'demEnvironmentIds' @::@ ['Text']
--
-- * 'demEnvironmentNames' @::@ ['Text']
--
-- * 'demIncludeDeleted' @::@ 'Maybe' 'Bool'
--
-- * 'demIncludedDeletedBackTo' @::@ 'Maybe' 'UTCTime'
--
-- * 'demVersionLabel' @::@ 'Maybe' 'Text'
--
describeEnvironments :: DescribeEnvironmentsMessage
describeEnvironments = DescribeEnvironmentsMessage
    { _demApplicationName       = Nothing
    , _demVersionLabel          = Nothing
    , _demEnvironmentIds        = mempty
    , _demEnvironmentNames      = mempty
    , _demIncludeDeleted        = Nothing
    , _demIncludedDeletedBackTo = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
demApplicationName :: Lens' DescribeEnvironmentsMessage (Maybe Text)
demApplicationName =
    lens _demApplicationName (\s a -> s { _demApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
demEnvironmentIds :: Lens' DescribeEnvironmentsMessage [Text]
demEnvironmentIds =
    lens _demEnvironmentIds (\s a -> s { _demEnvironmentIds = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
demEnvironmentNames :: Lens' DescribeEnvironmentsMessage [Text]
demEnvironmentNames =
    lens _demEnvironmentNames (\s a -> s { _demEnvironmentNames = a })

-- | Indicates whether to include deleted environments: true: Environments
-- that have been deleted after IncludedDeletedBackTo are displayed. false:
-- Do not include deleted environments.
demIncludeDeleted :: Lens' DescribeEnvironmentsMessage (Maybe Bool)
demIncludeDeleted =
    lens _demIncludeDeleted (\s a -> s { _demIncludeDeleted = a })

-- | If specified when IncludeDeleted is set to true, then environments
-- deleted after this date are displayed.
demIncludedDeletedBackTo :: Lens' DescribeEnvironmentsMessage (Maybe UTCTime)
demIncludedDeletedBackTo =
    lens _demIncludedDeletedBackTo
        (\s a -> s { _demIncludedDeletedBackTo = a })
            . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
demVersionLabel :: Lens' DescribeEnvironmentsMessage (Maybe Text)
demVersionLabel = lens _demVersionLabel (\s a -> s { _demVersionLabel = a })

instance ToPath DescribeEnvironmentsMessage where
    toPath = const "/"

instance ToQuery DescribeEnvironmentsMessage

newtype EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage
    { _edmEnvironments :: [EnvironmentDescription]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'EnvironmentDescriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edmEnvironments' @::@ ['EnvironmentDescription']
--
describeEnvironmentsResponse :: EnvironmentDescriptionsMessage
describeEnvironmentsResponse = EnvironmentDescriptionsMessage
    { _edmEnvironments = mempty
    }

-- | Returns an EnvironmentDescription list.
edmEnvironments :: Lens' EnvironmentDescriptionsMessage [EnvironmentDescription]
edmEnvironments = lens _edmEnvironments (\s a -> s { _edmEnvironments = a })

instance AWSRequest DescribeEnvironmentsMessage where
    type Sv DescribeEnvironmentsMessage = ElasticBeanstalk
    type Rs DescribeEnvironmentsMessage = EnvironmentDescriptionsMessage

    request  = post "DescribeEnvironments"
    response = xmlResponse $ \h x -> EnvironmentDescriptionsMessage
        <$> x %| "Environments"
