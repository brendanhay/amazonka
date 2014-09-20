{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and forces
-- a restart.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RebuildEnvironment &AuthParams
-- a7d6606e-f289-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    (
    -- * Request
      RebuildEnvironment
    -- ** Request constructor
    , rebuildEnvironment
    -- ** Request lenses
    , reEnvironmentId
    , reEnvironmentName

    -- * Response
    , RebuildEnvironmentResponse
    -- ** Response constructor
    , rebuildEnvironmentResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | 
data RebuildEnvironment = RebuildEnvironment
    { _reEnvironmentId :: Maybe Text
    , _reEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebuildEnvironment' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
rebuildEnvironment :: RebuildEnvironment
rebuildEnvironment = RebuildEnvironment
    { _reEnvironmentId = Nothing
    , _reEnvironmentName = Nothing
    }

-- | The ID of the environment to rebuild. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
reEnvironmentId :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentId = lens _reEnvironmentId (\s a -> s { _reEnvironmentId = a })

-- | The name of the environment to rebuild. Condition: You must specify either
-- this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
reEnvironmentName :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentName =
    lens _reEnvironmentName (\s a -> s { _reEnvironmentName = a })

instance ToQuery RebuildEnvironment where
    toQuery = genericQuery def

data RebuildEnvironmentResponse = RebuildEnvironmentResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebuildEnvironmentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse

instance AWSRequest RebuildEnvironment where
    type Sv RebuildEnvironment = ElasticBeanstalk
    type Rs RebuildEnvironment = RebuildEnvironmentResponse

    request = post "RebuildEnvironment"
    response _ = nullaryResponse RebuildEnvironmentResponse
