{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment where

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

-- | Minimum specification for a 'RebuildEnvironment' request.
rebuildEnvironment :: RebuildEnvironment
rebuildEnvironment = RebuildEnvironment
    { _remEnvironmentId = Nothing
    , _remEnvironmentName = Nothing
    }

data RebuildEnvironment = RebuildEnvironment
    { _remEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to rebuild. Condition: You must specify
      -- either this or an EnvironmentName, or both. If you do not specify
      -- either, AWS Elastic Beanstalk returns MissingRequiredParameter
      -- error.
    , _remEnvironmentName :: Maybe Text
      -- ^ The name of the environment to rebuild. Condition: You must
      -- specify either this or an EnvironmentId, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    } deriving (Generic)

instance ToQuery RebuildEnvironment where
    toQuery = genericToQuery def

instance AWSRequest RebuildEnvironment where
    type Sv RebuildEnvironment = ElasticBeanstalk
    type Rs RebuildEnvironment = RebuildEnvironmentResponse

    request = post "RebuildEnvironment"
    response _ _ = return (Right RebuildEnvironmentResponse)

data RebuildEnvironmentResponse = RebuildEnvironmentResponse
    deriving (Eq, Show, Generic)
