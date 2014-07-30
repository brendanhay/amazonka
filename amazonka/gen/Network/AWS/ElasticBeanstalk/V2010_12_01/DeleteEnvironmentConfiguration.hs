{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration
    { _decmApplicationName :: Text
      -- ^ The name of the application the environment is associated with.
    , _decmEnvironmentName :: Text
      -- ^ The name of the environment to delete the draft configuration
      -- from.
    } deriving (Generic)

instance ToQuery DeleteEnvironmentConfiguration where
    toQuery = genericToQuery def

instance AWSRequest DeleteEnvironmentConfiguration where
    type Sv DeleteEnvironmentConfiguration = ElasticBeanstalk
    type Rs DeleteEnvironmentConfiguration = DeleteEnvironmentConfigurationResponse

    request = post "DeleteEnvironmentConfiguration"
    response _ _ = return (Right DeleteEnvironmentConfigurationResponse)

data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse
    deriving (Eq, Show, Generic)
