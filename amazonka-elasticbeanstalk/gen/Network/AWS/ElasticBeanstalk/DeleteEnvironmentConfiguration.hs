{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the draft configuration associated with the running environment.
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The DeploymentStatus for the draft configuration indicates
-- whether the deployment is in process or has failed. The draft configuration
-- remains in existence until it is deleted with this action.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp &Operation=DeleteEnvironmentConfiguration
-- &AuthParams fdf76507-f26d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    (
    -- * Request
      DeleteEnvironmentConfiguration
    -- ** Request constructor
    , deleteEnvironmentConfiguration
    -- ** Request lenses
    , decApplicationName
    , decEnvironmentName

    -- * Response
    , DeleteEnvironmentConfigurationResponse
    -- ** Response constructor
    , deleteEnvironmentConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration
    { _decApplicationName :: Text
    , _decEnvironmentName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteEnvironmentConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @EnvironmentName ::@ @Text@
--
deleteEnvironmentConfiguration :: Text -- ^ 'decApplicationName'
                               -> Text -- ^ 'decEnvironmentName'
                               -> DeleteEnvironmentConfiguration
deleteEnvironmentConfiguration p1 p2 = DeleteEnvironmentConfiguration
    { _decApplicationName = p1
    , _decEnvironmentName = p2
    }

-- | The name of the application the environment is associated with.
decApplicationName :: Lens' DeleteEnvironmentConfiguration Text
decApplicationName =
    lens _decApplicationName (\s a -> s { _decApplicationName = a })

-- | The name of the environment to delete the draft configuration from.
decEnvironmentName :: Lens' DeleteEnvironmentConfiguration Text
decEnvironmentName =
    lens _decEnvironmentName (\s a -> s { _decEnvironmentName = a })

instance ToQuery DeleteEnvironmentConfiguration where
    toQuery = genericQuery def

data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteEnvironmentConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse

instance AWSRequest DeleteEnvironmentConfiguration where
    type Sv DeleteEnvironmentConfiguration = ElasticBeanstalk
    type Rs DeleteEnvironmentConfiguration = DeleteEnvironmentConfigurationResponse

    request = post "DeleteEnvironmentConfiguration"
    response _ = nullaryResponse DeleteEnvironmentConfigurationResponse
