{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DescribeApplicationVersions &AuthParams amazonaws.com sample.war
-- Version1 description SampleApp 2010-11-17T03:21:59.161Z
-- 2010-11-17T03:21:59.161Z 773cd80a-f26c-11df-8a78-9f77047e0d0c.
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

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | Result message containing a list of configuration descriptions.
data DescribeApplicationVersions = DescribeApplicationVersions
    { _dav1ApplicationName :: Maybe Text
    , _dav1VersionLabels :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApplicationVersions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @VersionLabels ::@ @[Text]@
--
describeApplicationVersions :: DescribeApplicationVersions
describeApplicationVersions = DescribeApplicationVersions
    { _dav1ApplicationName = Nothing
    , _dav1VersionLabels = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- only include ones that are associated with the specified application.
dav1ApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
dav1ApplicationName =
    lens _dav1ApplicationName (\s a -> s { _dav1ApplicationName = a })

-- | If specified, restricts the returned descriptions to only include ones that
-- have the specified version labels.
dav1VersionLabels :: Lens' DescribeApplicationVersions [Text]
dav1VersionLabels =
    lens _dav1VersionLabels (\s a -> s { _dav1VersionLabels = a })

instance ToQuery DescribeApplicationVersions where
    toQuery = genericQuery def

-- | Result message wrapping a list of application version descriptions.
newtype DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _davrApplicationVersions :: [ApplicationVersionDescription]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApplicationVersionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationVersions ::@ @[ApplicationVersionDescription]@
--
describeApplicationVersionsResponse :: DescribeApplicationVersionsResponse
describeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _davrApplicationVersions = mempty
    }

-- | A list of ApplicationVersionDescription .
davrApplicationVersions :: Lens' DescribeApplicationVersionsResponse [ApplicationVersionDescription]
davrApplicationVersions =
    lens _davrApplicationVersions
         (\s a -> s { _davrApplicationVersions = a })

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeApplicationVersions where
    type Sv DescribeApplicationVersions = ElasticBeanstalk
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse

    request = post "DescribeApplicationVersions"
    response _ = xmlResponse
