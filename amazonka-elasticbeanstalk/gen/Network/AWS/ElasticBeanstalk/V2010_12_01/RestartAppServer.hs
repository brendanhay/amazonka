{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Causes the environment to restart the application container server running
-- on each Amazon EC2 instance.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RestartAppServer &AuthParams
-- 90e8d1d5-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer
    (
    -- * Request
      RestartAppServer
    -- ** Request constructor
    , restartAppServer
    -- ** Request lenses
    , rasmEnvironmentId
    , rasmEnvironmentName

    -- * Response
    , RestartAppServerResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RestartAppServer' request.
restartAppServer :: RestartAppServer
restartAppServer = RestartAppServer
    { _rasmEnvironmentId = Nothing
    , _rasmEnvironmentName = Nothing
    }
{-# INLINE restartAppServer #-}

data RestartAppServer = RestartAppServer
    { _rasmEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to restart the server for. Condition:
      -- You must specify either this or an EnvironmentName, or both. If
      -- you do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _rasmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to restart the server for. Condition:
      -- You must specify either this or an EnvironmentId, or both. If you
      -- do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    } deriving (Show, Generic)

-- | The ID of the environment to restart the server for. Condition: You must
-- specify either this or an EnvironmentName, or both. If you do not specify
-- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
rasmEnvironmentId :: Lens' RestartAppServer (Maybe Text)
rasmEnvironmentId f x =
    f (_rasmEnvironmentId x)
        <&> \y -> x { _rasmEnvironmentId = y }
{-# INLINE rasmEnvironmentId #-}

-- | The name of the environment to restart the server for. Condition: You must
-- specify either this or an EnvironmentId, or both. If you do not specify
-- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
rasmEnvironmentName :: Lens' RestartAppServer (Maybe Text)
rasmEnvironmentName f x =
    f (_rasmEnvironmentName x)
        <&> \y -> x { _rasmEnvironmentName = y }
{-# INLINE rasmEnvironmentName #-}

instance ToQuery RestartAppServer where
    toQuery = genericQuery def

data RestartAppServerResponse = RestartAppServerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RestartAppServer where
    type Sv RestartAppServer = ElasticBeanstalk
    type Rs RestartAppServer = RestartAppServerResponse

    request = post "RestartAppServer"
    response _ = nullaryResponse RestartAppServerResponse
