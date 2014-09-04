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
    , mkDescribeApplicationVersionsMessage
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApplicationVersions' request.
mkDescribeApplicationVersionsMessage :: DescribeApplicationVersions
mkDescribeApplicationVersionsMessage = DescribeApplicationVersions
    { _davnApplicationName = Nothing
    , _davnVersionLabels = mempty
    }
{-# INLINE mkDescribeApplicationVersionsMessage #-}

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
davnApplicationName :: Lens' DescribeApplicationVersions (Maybe Text)
davnApplicationName = lens _davnApplicationName (\s a -> s { _davnApplicationName = a })
{-# INLINE davnApplicationName #-}

-- | If specified, restricts the returned descriptions to only include ones that
-- have the specified version labels.
davnVersionLabels :: Lens' DescribeApplicationVersions ([Text])
davnVersionLabels = lens _davnVersionLabels (\s a -> s { _davnVersionLabels = a })
{-# INLINE davnVersionLabels #-}

instance ToQuery DescribeApplicationVersions where
    toQuery = genericQuery def

newtype DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _avdnApplicationVersions :: [ApplicationVersionDescription]
      -- ^ A list of ApplicationVersionDescription .
    } deriving (Show, Generic)

-- | A list of ApplicationVersionDescription .
avdnApplicationVersions :: Lens' DescribeApplicationVersionsResponse ([ApplicationVersionDescription])
avdnApplicationVersions = lens _avdnApplicationVersions (\s a -> s { _avdnApplicationVersions = a })
{-# INLINE avdnApplicationVersions #-}

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeApplicationVersions where
    type Sv DescribeApplicationVersions = ElasticBeanstalk
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse

    request = post "DescribeApplicationVersions"
    response _ = xmlResponse
