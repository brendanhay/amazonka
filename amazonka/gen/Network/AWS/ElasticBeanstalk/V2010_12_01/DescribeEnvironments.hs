{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions for existing environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &IncludeDeleted=true &IncludedDeletedBackTo=2008-11-05T06%3A00%3A00Z
-- &Operation=DescribeEnvironments &AuthParams Version1 Available SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Green e-icsgecu3wf
-- 2010-11-17T04:01:40.668Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 44790c68-f260-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEnvironments' request.
describeEnvironments :: DescribeEnvironments
describeEnvironments = DescribeEnvironments
    { _denApplicationName = Nothing
    , _denEnvironmentIds = mempty
    , _denEnvironmentNames = mempty
    , _denIncludeDeleted = Nothing
    , _denIncludedDeletedBackTo = Nothing
    , _denVersionLabel = Nothing
    }

data DescribeEnvironments = DescribeEnvironments
    { _denApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that are associated with this
      -- application.
    , _denEnvironmentIds :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that have the specified IDs.
    , _denEnvironmentNames :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that have the specified names.
    , _denIncludeDeleted :: Maybe Bool
      -- ^ Indicates whether to include deleted environments: true:
      -- Environments that have been deleted after IncludedDeletedBackTo
      -- are displayed. false: Do not include deleted environments.
    , _denIncludedDeletedBackTo :: Maybe ISO8601
      -- ^ If specified when IncludeDeleted is set to true, then
      -- environments deleted after this date are displayed.
    , _denVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that are associated with this
      -- application version.
    } deriving (Generic)

makeLenses ''DescribeEnvironments

instance ToQuery DescribeEnvironments where
    toQuery = genericToQuery def

data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _ednEnvironments :: [EnvironmentDescription]
      -- ^ Returns an EnvironmentDescription list.
    } deriving (Generic)

makeLenses ''DescribeEnvironmentsResponse

instance FromXML DescribeEnvironmentsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironments where
    type Sv DescribeEnvironments = ElasticBeanstalk
    type Rs DescribeEnvironments = DescribeEnvironmentsResponse

    request = post "DescribeEnvironments"
    response _ = xmlResponse
