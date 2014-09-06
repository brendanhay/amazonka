{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resources for this environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=DescribeEnvironmentResources
-- &AuthParams elasticbeanstalk-SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-hbAc8cSZH7
-- elasticbeanstalk-SampleAppVersion-us-east-1c SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-us-east-1c
-- e1cb7b96-f287-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
    (
    -- * Request
      DescribeEnvironmentResources
    -- ** Request constructor
    , mkDescribeEnvironmentResources
    -- ** Request lenses
    , derEnvironmentId
    , derEnvironmentName

    -- * Response
    , DescribeEnvironmentResourcesResponse
    -- ** Response lenses
    , derrsEnvironmentResources
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DescribeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId :: Maybe Text
    , _derEnvironmentName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEnvironmentResources' request.
mkDescribeEnvironmentResources :: DescribeEnvironmentResources
mkDescribeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId = Nothing
    , _derEnvironmentName = Nothing
    }
{-# INLINE mkDescribeEnvironmentResources #-}

-- | The ID of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
derEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentId =
    lens _derEnvironmentId (\s a -> s { _derEnvironmentId = a })
{-# INLINE derEnvironmentId #-}

-- | The name of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentId, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
derEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentName =
    lens _derEnvironmentName (\s a -> s { _derEnvironmentName = a })
{-# INLINE derEnvironmentName #-}

instance ToQuery DescribeEnvironmentResources where
    toQuery = genericQuery def

-- | Result message containing a list of environment resource descriptions.
newtype DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _derrsEnvironmentResources :: Maybe EnvironmentResourceDescription
    } deriving (Show, Generic)

-- | A list of EnvironmentResourceDescription.
derrsEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrsEnvironmentResources =
    lens _derrsEnvironmentResources
         (\s a -> s { _derrsEnvironmentResources = a })
{-# INLINE derrsEnvironmentResources #-}

instance FromXML DescribeEnvironmentResourcesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironmentResources where
    type Sv DescribeEnvironmentResources = ElasticBeanstalk
    type Rs DescribeEnvironmentResources = DescribeEnvironmentResourcesResponse

    request = post "DescribeEnvironmentResources"
    response _ = xmlResponse
