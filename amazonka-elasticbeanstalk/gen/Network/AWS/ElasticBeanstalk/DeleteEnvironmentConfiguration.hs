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
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    (
    -- * Request
      DeleteEnvironmentConfigurationMessage
    -- ** Request constructor
    , deleteEnvironmentConfigurationMessage
    -- ** Request lenses
    , decmApplicationName
    , decmEnvironmentName

    -- * Response
    , DeleteEnvironmentConfigurationResponse
    -- ** Response constructor
    , deleteEnvironmentConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DeleteEnvironmentConfigurationMessage = DeleteEnvironmentConfigurationMessage
    { _decmApplicationName :: Text
    , _decmEnvironmentName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteEnvironmentConfigurationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decmApplicationName' @::@ 'Text'
--
-- * 'decmEnvironmentName' @::@ 'Text'
--
deleteEnvironmentConfigurationMessage :: Text -- ^ 'decmApplicationName'
                                      -> Text -- ^ 'decmEnvironmentName'
                                      -> DeleteEnvironmentConfigurationMessage
deleteEnvironmentConfigurationMessage p1 p2 = DeleteEnvironmentConfigurationMessage
    { _decmApplicationName = p1
    , _decmEnvironmentName = p2
    }

-- | The name of the application the environment is associated with.
decmApplicationName :: Lens' DeleteEnvironmentConfigurationMessage Text
decmApplicationName =
    lens _decmApplicationName (\s a -> s { _decmApplicationName = a })

-- | The name of the environment to delete the draft configuration from.
decmEnvironmentName :: Lens' DeleteEnvironmentConfigurationMessage Text
decmEnvironmentName =
    lens _decmEnvironmentName (\s a -> s { _decmEnvironmentName = a })

instance ToPath DeleteEnvironmentConfigurationMessage where
    toPath = const "/"

instance ToQuery DeleteEnvironmentConfigurationMessage

data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse

-- | 'DeleteEnvironmentConfigurationResponse' constructor.
deleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse

instance AWSRequest DeleteEnvironmentConfigurationMessage where
    type Sv DeleteEnvironmentConfigurationMessage = ElasticBeanstalk
    type Rs DeleteEnvironmentConfigurationMessage = DeleteEnvironmentConfigurationResponse

    request  = post "DeleteEnvironmentConfiguration"
    response = const (nullaryResponse DeleteEnvironmentConfigurationResponse)
