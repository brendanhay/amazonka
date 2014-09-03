{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions
    (
    -- * Request
      DescribeApplicationVersions
    -- ** Request constructor
    , describeApplicationVersions
    -- ** Request lenses
    , davnApplicationName
    , davnVersionLabels

    -- * Response
    , DescribeApplicationVersionsResponse
    -- ** Response lenses
    , avdnApplicationVersions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeApplicationVersions' request.
describeApplicationVersions :: DescribeApplicationVersions
describeApplicationVersions = DescribeApplicationVersions
    { _davnApplicationName = Nothing
    , _davnVersionLabels = mempty
    }

data DescribeApplicationVersions = DescribeApplicationVersions
    { _davnApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to only include ones that are associated with the
      -- specified application.
    , _davnVersionLabels :: [Text]
      -- ^ If specified, restricts the returned descriptions to only include
      -- ones that have the specified version labels.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- only include ones that are associated with the specified application.
davnApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeApplicationVersions
    -> f DescribeApplicationVersions
davnApplicationName f x =
    (\y -> x { _davnApplicationName = y })
       <$> f (_davnApplicationName x)
{-# INLINE davnApplicationName #-}

-- | If specified, restricts the returned descriptions to only include ones that
-- have the specified version labels.
davnVersionLabels
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeApplicationVersions
    -> f DescribeApplicationVersions
davnVersionLabels f x =
    (\y -> x { _davnVersionLabels = y })
       <$> f (_davnVersionLabels x)
{-# INLINE davnVersionLabels #-}

instance ToQuery DescribeApplicationVersions where
    toQuery = genericQuery def

data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _avdnApplicationVersions :: [ApplicationVersionDescription]
      -- ^ A list of ApplicationVersionDescription .
    } deriving (Show, Generic)

-- | A list of ApplicationVersionDescription .
avdnApplicationVersions
    :: Functor f
    => ([ApplicationVersionDescription]
    -> f ([ApplicationVersionDescription]))
    -> DescribeApplicationVersionsResponse
    -> f DescribeApplicationVersionsResponse
avdnApplicationVersions f x =
    (\y -> x { _avdnApplicationVersions = y })
       <$> f (_avdnApplicationVersions x)
{-# INLINE avdnApplicationVersions #-}

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeApplicationVersions where
    type Sv DescribeApplicationVersions = ElasticBeanstalk
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse

    request = post "DescribeApplicationVersions"
    response _ = xmlResponse
