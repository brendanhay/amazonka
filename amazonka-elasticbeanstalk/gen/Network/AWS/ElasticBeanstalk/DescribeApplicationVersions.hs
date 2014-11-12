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
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    (
    -- * Request
      DescribeApplicationVersionsMessage
    -- ** Request constructor
    , describeApplicationVersions
    -- ** Request lenses
    , davm1ApplicationName
    , davm1VersionLabels

    -- * Response
    , ApplicationVersionDescriptionsMessage
    -- ** Response constructor
    , describeApplicationVersionsResponse
    -- ** Response lenses
    , avdmApplicationVersions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeApplicationVersionsMessage = DescribeApplicationVersionsMessage
    { _davm1ApplicationName :: Maybe Text
    , _davm1VersionLabels   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeApplicationVersionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davm1ApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'davm1VersionLabels' @::@ ['Text']
--
describeApplicationVersions :: DescribeApplicationVersionsMessage
describeApplicationVersions = DescribeApplicationVersionsMessage
    { _davm1ApplicationName = Nothing
    , _davm1VersionLabels   = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include ones that are associated with the specified application.
davm1ApplicationName :: Lens' DescribeApplicationVersionsMessage (Maybe Text)
davm1ApplicationName =
    lens _davm1ApplicationName (\s a -> s { _davm1ApplicationName = a })

-- | If specified, restricts the returned descriptions to only include ones
-- that have the specified version labels.
davm1VersionLabels :: Lens' DescribeApplicationVersionsMessage [Text]
davm1VersionLabels =
    lens _davm1VersionLabels (\s a -> s { _davm1VersionLabels = a })

instance ToQuery DescribeApplicationVersionsMessage

instance ToPath DescribeApplicationVersionsMessage where
    toPath = const "/"

newtype ApplicationVersionDescriptionsMessage = ApplicationVersionDescriptionsMessage
    { _avdmApplicationVersions :: [ApplicationVersionDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList ApplicationVersionDescriptionsMessage where
    type Item ApplicationVersionDescriptionsMessage = ApplicationVersionDescription

    fromList = ApplicationVersionDescriptionsMessage . fromList
    toList   = toList . _avdmApplicationVersions

-- | 'ApplicationVersionDescriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdmApplicationVersions' @::@ ['ApplicationVersionDescription']
--
describeApplicationVersionsResponse :: ApplicationVersionDescriptionsMessage
describeApplicationVersionsResponse = ApplicationVersionDescriptionsMessage
    { _avdmApplicationVersions = mempty
    }

-- | A list of ApplicationVersionDescription .
avdmApplicationVersions :: Lens' ApplicationVersionDescriptionsMessage [ApplicationVersionDescription]
avdmApplicationVersions =
    lens _avdmApplicationVersions (\s a -> s { _avdmApplicationVersions = a })

instance FromXML ApplicationVersionDescriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescriptionsMessage"

instance AWSRequest DescribeApplicationVersionsMessage where
    type Sv DescribeApplicationVersionsMessage = ElasticBeanstalk
    type Rs DescribeApplicationVersionsMessage = ApplicationVersionDescriptionsMessage

    request  = post "DescribeApplicationVersions"
    response = xmlResponse $ \h x -> ApplicationVersionDescriptionsMessage
        <$> x %| "ApplicationVersions"
