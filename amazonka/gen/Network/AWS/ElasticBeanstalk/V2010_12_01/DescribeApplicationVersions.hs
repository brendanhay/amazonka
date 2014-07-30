{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

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
    } deriving (Generic)

instance ToQuery DescribeApplicationVersions where
    toQuery = genericToQuery def

instance AWSRequest DescribeApplicationVersions where
    type Sv DescribeApplicationVersions = ElasticBeanstalk
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse

    request = post "DescribeApplicationVersions"
    response _ = xmlResponse

data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { _avdoApplicationVersions :: [ApplicationVersionDescription]
      -- ^ A list of ApplicationVersionDescription .
    } deriving (Generic)

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions
