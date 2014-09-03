{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified application along with all associated versions and
-- configurations. The application versions will not be deleted from your
-- Amazon S3 bucket. You cannot delete an application that has a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DeleteApplication &AuthParams
-- 1f155abd-f1d7-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication
    (
    -- * Request
      DeleteApplication
    -- ** Request constructor
    , deleteApplication
    -- ** Request lenses
    , damApplicationName
    , damTerminateEnvByForce

    -- * Response
    , DeleteApplicationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteApplication' request.
deleteApplication :: Text -- ^ 'damApplicationName'
                  -> DeleteApplication
deleteApplication p1 = DeleteApplication
    { _damApplicationName = p1
    , _damTerminateEnvByForce = Nothing
    }

data DeleteApplication = DeleteApplication
    { _damApplicationName :: Text
      -- ^ The name of the application to delete.
    , _damTerminateEnvByForce :: Maybe Bool
      -- ^ When set to true, running environments will be terminated before
      -- deleting the application.
    } deriving (Show, Generic)

-- | The name of the application to delete.
damApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteApplication
    -> f DeleteApplication
damApplicationName f x =
    (\y -> x { _damApplicationName = y })
       <$> f (_damApplicationName x)
{-# INLINE damApplicationName #-}

-- | When set to true, running environments will be terminated before deleting
-- the application.
damTerminateEnvByForce
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DeleteApplication
    -> f DeleteApplication
damTerminateEnvByForce f x =
    (\y -> x { _damTerminateEnvByForce = y })
       <$> f (_damTerminateEnvByForce x)
{-# INLINE damTerminateEnvByForce #-}

instance ToQuery DeleteApplication where
    toQuery = genericQuery def

data DeleteApplicationResponse = DeleteApplicationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteApplication where
    type Sv DeleteApplication = ElasticBeanstalk
    type Rs DeleteApplication = DeleteApplicationResponse

    request = post "DeleteApplication"
    response _ = nullaryResponse DeleteApplicationResponse
