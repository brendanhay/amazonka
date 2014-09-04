{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments
    (
    -- * Request
      DescribeEnvironments
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
    , DescribeEnvironmentsResponse
    -- ** Response lenses
    , edmEnvironments
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEnvironments' request.
describeEnvironments :: DescribeEnvironments
describeEnvironments = DescribeEnvironments
    { _demApplicationName = Nothing
    , _demEnvironmentIds = mempty
    , _demEnvironmentNames = mempty
    , _demIncludeDeleted = Nothing
    , _demIncludedDeletedBackTo = Nothing
    , _demVersionLabel = Nothing
    }
{-# INLINE describeEnvironments #-}

data DescribeEnvironments = DescribeEnvironments
    { _demApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that are associated with this
      -- application.
    , _demEnvironmentIds :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that have the specified IDs.
    , _demEnvironmentNames :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that have the specified names.
    , _demIncludeDeleted :: Maybe Bool
      -- ^ Indicates whether to include deleted environments: true:
      -- Environments that have been deleted after IncludedDeletedBackTo
      -- are displayed. false: Do not include deleted environments.
    , _demIncludedDeletedBackTo :: Maybe ISO8601
      -- ^ If specified when IncludeDeleted is set to true, then
      -- environments deleted after this date are displayed.
    , _demVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those that are associated with this
      -- application version.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that are associated with this application.
demApplicationName :: Lens' DescribeEnvironments (Maybe Text)
demApplicationName f x =
    f (_demApplicationName x)
        <&> \y -> x { _demApplicationName = y }
{-# INLINE demApplicationName #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that have the specified IDs.
demEnvironmentIds :: Lens' DescribeEnvironments ([Text])
demEnvironmentIds f x =
    f (_demEnvironmentIds x)
        <&> \y -> x { _demEnvironmentIds = y }
{-# INLINE demEnvironmentIds #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that have the specified names.
demEnvironmentNames :: Lens' DescribeEnvironments ([Text])
demEnvironmentNames f x =
    f (_demEnvironmentNames x)
        <&> \y -> x { _demEnvironmentNames = y }
{-# INLINE demEnvironmentNames #-}

-- | Indicates whether to include deleted environments: true: Environments that
-- have been deleted after IncludedDeletedBackTo are displayed. false: Do not
-- include deleted environments.
demIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
demIncludeDeleted f x =
    f (_demIncludeDeleted x)
        <&> \y -> x { _demIncludeDeleted = y }
{-# INLINE demIncludeDeleted #-}

-- | If specified when IncludeDeleted is set to true, then environments deleted
-- after this date are displayed.
demIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe ISO8601)
demIncludedDeletedBackTo f x =
    f (_demIncludedDeletedBackTo x)
        <&> \y -> x { _demIncludedDeletedBackTo = y }
{-# INLINE demIncludedDeletedBackTo #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that are associated with this application version.
demVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
demVersionLabel f x =
    f (_demVersionLabel x)
        <&> \y -> x { _demVersionLabel = y }
{-# INLINE demVersionLabel #-}

instance ToQuery DescribeEnvironments where
    toQuery = genericQuery def

data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _edmEnvironments :: [EnvironmentDescription]
      -- ^ Returns an EnvironmentDescription list.
    } deriving (Show, Generic)

-- | Returns an EnvironmentDescription list.
edmEnvironments :: Lens' DescribeEnvironmentsResponse ([EnvironmentDescription])
edmEnvironments f x =
    f (_edmEnvironments x)
        <&> \y -> x { _edmEnvironments = y }
{-# INLINE edmEnvironments #-}

instance FromXML DescribeEnvironmentsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironments where
    type Sv DescribeEnvironments = ElasticBeanstalk
    type Rs DescribeEnvironments = DescribeEnvironmentsResponse

    request = post "DescribeEnvironments"
    response _ = xmlResponse
