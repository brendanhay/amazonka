{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions for existing application versions.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeApplicationVersions.html>
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    (
    -- * Request
      DescribeApplicationVersions
    -- ** Request constructor
    , describeApplicationVersions
    -- ** Request lenses
    , dav1ApplicationName
    , dav1VersionLabels

    -- * Response
    , DescribeApplicationVersionsResponse
    -- ** Response constructor
    , describeApplicationVersionsResponse
    -- ** Response lenses
    , davrApplicationVersions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DescribeApplicationVersions = DescribeApplicationVersions
    { _dav1ApplicationName :: Maybe Text
    , _dav1VersionLabels   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeApplicationVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dav1ApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'dav1VersionLabels' @::@ ['Text']
--
describeApplicationVersions :: DescribeApplicationVersions
describeApplicationVersions = DescribeApplicationVersions
    { _dav1ApplicationName = Nothing
    , _dav1VersionLabels   = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include ones that are associated with the specified application.
dav1ApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
dav1ApplicationName =
    lens _dav1ApplicationName (\s a -> s { _dav1ApplicationName = a })

-- | If specified, restricts the returned descriptions to only include ones
-- that have the specified version labels.
dav1VersionLabels :: Lens' DescribeApplicationVersions [Text]
dav1VersionLabels =
    lens _dav1VersionLabels (\s a -> s { _dav1VersionLabels = a })

newtype DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _davrApplicationVersions :: [ApplicationVersionDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeApplicationVersionsResponse where
    type Item DescribeApplicationVersionsResponse = ApplicationVersionDescription

    fromList = DescribeApplicationVersionsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _davrApplicationVersions

-- | 'DescribeApplicationVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davrApplicationVersions' @::@ ['ApplicationVersionDescription']
--
describeApplicationVersionsResponse :: DescribeApplicationVersionsResponse
describeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _davrApplicationVersions = mempty
    }

-- | A list of ApplicationVersionDescription .
davrApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrApplicationVersions =
    lens _davrApplicationVersions (\s a -> s { _davrApplicationVersions = a })

instance AWSRequest DescribeApplicationVersions where
    type Sv DescribeApplicationVersions = ElasticBeanstalk
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse

    request  = post "DescribeApplicationVersions"
    response = xmlResponse

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeApplicationVersionsResponse"

instance ToPath DescribeApplicationVersions where
    toPath = const "/"

instance ToHeaders DescribeApplicationVersions

instance ToQuery DescribeApplicationVersions
