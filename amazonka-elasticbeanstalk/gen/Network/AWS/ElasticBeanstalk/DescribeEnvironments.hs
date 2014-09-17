{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &IncludeDeleted=true &IncludedDeletedBackTo=2008-11-05T06%3A00%3A00Z
-- &Operation=DescribeEnvironments &AuthParams Version1 Available SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Green e-icsgecu3wf
-- 2010-11-17T04:01:40.668Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 44790c68-f260-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    (
    -- * Request
      DescribeEnvironments
    -- ** Request constructor
    , mkDescribeEnvironments
    -- ** Request lenses
    , deApplicationName
    , deVersionLabel
    , deEnvironmentIds
    , deEnvironmentNames
    , deIncludeDeleted
    , deIncludedDeletedBackTo

    -- * Response
    , DescribeEnvironmentsResponse
    -- ** Response constructor
    , mkDescribeEnvironmentsResponse
    -- ** Response lenses
    , der1Environments
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DescribeEnvironments = DescribeEnvironments
    { _deApplicationName :: Maybe Text
    , _deVersionLabel :: Maybe Text
    , _deEnvironmentIds :: [Text]
    , _deEnvironmentNames :: [Text]
    , _deIncludeDeleted :: Maybe Bool
    , _deIncludedDeletedBackTo :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEnvironments' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @EnvironmentIds ::@ @[Text]@
--
-- * @EnvironmentNames ::@ @[Text]@
--
-- * @IncludeDeleted ::@ @Maybe Bool@
--
-- * @IncludedDeletedBackTo ::@ @Maybe ISO8601@
--
mkDescribeEnvironments :: DescribeEnvironments
mkDescribeEnvironments = DescribeEnvironments
    { _deApplicationName = Nothing
    , _deVersionLabel = Nothing
    , _deEnvironmentIds = mempty
    , _deEnvironmentNames = mempty
    , _deIncludeDeleted = Nothing
    , _deIncludedDeletedBackTo = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that are associated with this application.
deApplicationName :: Lens' DescribeEnvironments (Maybe Text)
deApplicationName =
    lens _deApplicationName (\s a -> s { _deApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that are associated with this application version.
deVersionLabel :: Lens' DescribeEnvironments (Maybe Text)
deVersionLabel = lens _deVersionLabel (\s a -> s { _deVersionLabel = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that have the specified IDs.
deEnvironmentIds :: Lens' DescribeEnvironments [Text]
deEnvironmentIds =
    lens _deEnvironmentIds (\s a -> s { _deEnvironmentIds = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those that have the specified names.
deEnvironmentNames :: Lens' DescribeEnvironments [Text]
deEnvironmentNames =
    lens _deEnvironmentNames (\s a -> s { _deEnvironmentNames = a })

-- | Indicates whether to include deleted environments: true: Environments that
-- have been deleted after IncludedDeletedBackTo are displayed. false: Do not
-- include deleted environments.
deIncludeDeleted :: Lens' DescribeEnvironments (Maybe Bool)
deIncludeDeleted =
    lens _deIncludeDeleted (\s a -> s { _deIncludeDeleted = a })

-- | If specified when IncludeDeleted is set to true, then environments deleted
-- after this date are displayed.
deIncludedDeletedBackTo :: Lens' DescribeEnvironments (Maybe ISO8601)
deIncludedDeletedBackTo =
    lens _deIncludedDeletedBackTo
         (\s a -> s { _deIncludedDeletedBackTo = a })

instance ToQuery DescribeEnvironments where
    toQuery = genericQuery def

-- | Result message containing a list of environment descriptions.
newtype DescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _der1Environments :: [EnvironmentDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEnvironmentsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Environments ::@ @[EnvironmentDescription]@
--
mkDescribeEnvironmentsResponse :: DescribeEnvironmentsResponse
mkDescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { _der1Environments = mempty
    }

-- | Returns an EnvironmentDescription list.
der1Environments :: Lens' DescribeEnvironmentsResponse [EnvironmentDescription]
der1Environments =
    lens _der1Environments (\s a -> s { _der1Environments = a })

instance FromXML DescribeEnvironmentsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironments where
    type Sv DescribeEnvironments = ElasticBeanstalk
    type Rs DescribeEnvironments = DescribeEnvironmentsResponse

    request = post "DescribeEnvironments"
    response _ = xmlResponse
