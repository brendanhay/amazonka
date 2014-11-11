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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resources for this environment.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    (
    -- * Request
      DescribeEnvironmentResourcesMessage
    -- ** Request constructor
    , describeEnvironmentResourcesMessage
    -- ** Request lenses
    , dermEnvironmentId
    , dermEnvironmentName

    -- * Response
    , EnvironmentResourceDescriptionsMessage
    -- ** Response constructor
    , environmentResourceDescriptionsMessage
    -- ** Response lenses
    , erdmEnvironmentResources
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeEnvironmentResourcesMessage = DescribeEnvironmentResourcesMessage
    { _dermEnvironmentId   :: Maybe Text
    , _dermEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEnvironmentResourcesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dermEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'dermEnvironmentName' @::@ 'Maybe' 'Text'
--
describeEnvironmentResourcesMessage :: DescribeEnvironmentResourcesMessage
describeEnvironmentResourcesMessage = DescribeEnvironmentResourcesMessage
    { _dermEnvironmentId   = Nothing
    , _dermEnvironmentName = Nothing
    }

-- | The ID of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do
-- not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
dermEnvironmentId :: Lens' DescribeEnvironmentResourcesMessage (Maybe Text)
dermEnvironmentId =
    lens _dermEnvironmentId (\s a -> s { _dermEnvironmentId = a })

-- | The name of the environment to retrieve AWS resource usage data.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
dermEnvironmentName :: Lens' DescribeEnvironmentResourcesMessage (Maybe Text)
dermEnvironmentName =
    lens _dermEnvironmentName (\s a -> s { _dermEnvironmentName = a })
instance ToQuery DescribeEnvironmentResourcesMessage

instance ToPath DescribeEnvironmentResourcesMessage where
    toPath = const "/"

newtype EnvironmentResourceDescriptionsMessage = EnvironmentResourceDescriptionsMessage
    { _erdmEnvironmentResources :: Maybe EnvironmentResourceDescription
    } deriving (Eq, Show, Generic)

-- | 'EnvironmentResourceDescriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdmEnvironmentResources' @::@ 'Maybe' 'EnvironmentResourceDescription'
--
environmentResourceDescriptionsMessage :: EnvironmentResourceDescriptionsMessage
environmentResourceDescriptionsMessage = EnvironmentResourceDescriptionsMessage
    { _erdmEnvironmentResources = Nothing
    }

-- | A list of EnvironmentResourceDescription.
erdmEnvironmentResources :: Lens' EnvironmentResourceDescriptionsMessage (Maybe EnvironmentResourceDescription)
erdmEnvironmentResources =
    lens _erdmEnvironmentResources
        (\s a -> s { _erdmEnvironmentResources = a })
instance FromXML EnvironmentResourceDescriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescriptionsMessage"

instance AWSRequest DescribeEnvironmentResourcesMessage where
    type Sv DescribeEnvironmentResourcesMessage = ElasticBeanstalk
    type Rs DescribeEnvironmentResourcesMessage = EnvironmentResourceDescriptionsMessage

    request  = post "DescribeEnvironmentResources"
    response = xmlResponse $ \h x -> EnvironmentResourceDescriptionsMessage
        <$> x %| "EnvironmentResources"
