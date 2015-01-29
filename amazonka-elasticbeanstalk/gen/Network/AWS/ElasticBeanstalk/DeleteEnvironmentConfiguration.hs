{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using 'DescribeConfigurationSettings' while the update is in progress or if the update fails. The 'DeploymentStatus'
-- for the draft configuration indicates whether the deployment is in process or
-- has failed. The draft configuration remains in existence until it is deleted
-- with this action.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteEnvironmentConfiguration.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration
    { _decApplicationName :: Text
    , _decEnvironmentName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteEnvironmentConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decApplicationName' @::@ 'Text'
--
-- * 'decEnvironmentName' @::@ 'Text'
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

data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteEnvironmentConfigurationResponse' constructor.
deleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse

instance ToPath DeleteEnvironmentConfiguration where
    toPath = const "/"

instance ToQuery DeleteEnvironmentConfiguration where
    toQuery DeleteEnvironmentConfiguration{..} = mconcat
        [ "ApplicationName" =? _decApplicationName
        , "EnvironmentName" =? _decEnvironmentName
        ]

instance ToHeaders DeleteEnvironmentConfiguration

instance AWSRequest DeleteEnvironmentConfiguration where
    type Sv DeleteEnvironmentConfiguration = ElasticBeanstalk
    type Rs DeleteEnvironmentConfiguration = DeleteEnvironmentConfigurationResponse

    request  = post "DeleteEnvironmentConfiguration"
    response = nullResponse DeleteEnvironmentConfigurationResponse
