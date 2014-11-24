{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironments.html>
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    (
    -- * Request
      DescribeEnvironments
    -- ** Request constructor
    , describeEnvironments
    -- ** Request lenses
    , de1ApplicationName
    , de1EnvironmentIds
    , de1EnvironmentNames
    , de1IncludeDeleted
    , de1IncludedDeletedBackTo
    , de1VersionLabel

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , describeEnvironmentsResponse
    -- ** Response lenses
    , derEnvironments
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DescribeEnvironments = DescribeEnvironments
    { _de1ApplicationName       :: Maybe Text
    , _de1EnvironmentIds        :: List "EnvironmentIds" Text
    , _de1EnvironmentNames      :: List "EnvironmentNames" Text
    , _de1IncludeDeleted        :: Maybe Bool
    , _de1IncludedDeletedBackTo :: Maybe RFC822
    , _de1VersionLabel          :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeEnvironments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'de1ApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'de1EnvironmentIds' @::@ ['Text']
--
-- * 'de1EnvironmentNames' @::@ ['Text']
--
-- * 'de1IncludeDeleted' @::@ 'Maybe' 'Bool'
--
-- * 'de1IncludedDeletedBackTo' @::@ 'Maybe' 'UTCTime'
--
-- * 'de1VersionLabel' @::@ 'Maybe' 'Text'
--
describeEnvironments :: DescribeEnvironments
describeEnvironments = DescribeEnvironments
    { _de1ApplicationName       = Nothing
    , _de1VersionLabel          = Nothing
    , _de1EnvironmentIds        = mempty
    , _de1EnvironmentNames      = mempty
    , _de1IncludeDeleted        = Nothing
    , _de1IncludedDeletedBackTo = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
de1ApplicationName :: Lens' DescribeEnvironments (Maybe Text)
de1ApplicationName =
    lens _de1ApplicationName (\s a -> s { _de1ApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
de1EnvironmentIds :: Lens' DescribeEnvironments [Text]
de1EnvironmentIds =
    lens _de1EnvironmentIds (\s a -> s { _de1EnvironmentIds = a })
        . _List

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
de1EnvironmentNames :: Lens' DescribeEnvironments [Text]
de1EnvironmentNames =
    lens _de1EnvironmentNames (\s a -> s { _de1EnvironmentNames = a })
        . _List

-- | Indicates whether to include deleted environments: @true@: Environments
-- that have been deleted after @IncludedDeletedBackTo@ are displayed.
-- @false@: Do not include deleted environments.
de1IncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
de1IncludeDeleted =
    lens _de1IncludeDeleted (\s a -> s { _de1IncludeDeleted = a })

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
de1IncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe UTCTime)
de1IncludedDeletedBackTo =
    lens _de1IncludedDeletedBackTo
        (\s a -> s { _de1IncludedDeletedBackTo = a })
            . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
de1VersionLabel :: Lens' DescribeEnvironments (Maybe Text)
de1VersionLabel = lens _de1VersionLabel (\s a -> s { _de1VersionLabel = a })

newtype DescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _derEnvironments :: List "Environments" EnvironmentDescription
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeEnvironmentsResponse where
    type Item DescribeEnvironmentsResponse = EnvironmentDescription

    fromList = DescribeEnvironmentsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _derEnvironments

-- | 'DescribeEnvironmentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEnvironments' @::@ ['EnvironmentDescription']
--
describeEnvironmentsResponse :: DescribeEnvironmentsResponse
describeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _derEnvironments = mempty
    }

-- | Returns an EnvironmentDescription> list.
derEnvironments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
derEnvironments = lens _derEnvironments (\s a -> s { _derEnvironments = a }) . _List

instance ToPath DescribeEnvironments where
    toPath = const "/"

instance ToQuery DescribeEnvironments where
    toQuery DescribeEnvironments{..} = mconcat
        [ "ApplicationName"       =? _de1ApplicationName
        , "EnvironmentIds"        =? _de1EnvironmentIds
        , "EnvironmentNames"      =? _de1EnvironmentNames
        , "IncludeDeleted"        =? _de1IncludeDeleted
        , "IncludedDeletedBackTo" =? _de1IncludedDeletedBackTo
        , "VersionLabel"          =? _de1VersionLabel
        ]

instance ToHeaders DescribeEnvironments

instance AWSRequest DescribeEnvironments where
    type Sv DescribeEnvironments = ElasticBeanstalk
    type Rs DescribeEnvironments = DescribeEnvironmentsResponse

    request  = post "DescribeEnvironments"
    response = xmlResponse

instance FromXML DescribeEnvironmentsResponse where
    parseXML = withElement "DescribeEnvironmentsResult" $ \x -> DescribeEnvironmentsResponse
        <$> x .@  "Environments"
