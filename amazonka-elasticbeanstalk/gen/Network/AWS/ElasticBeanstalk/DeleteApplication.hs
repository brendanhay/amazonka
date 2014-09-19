{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplication
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
module Network.AWS.ElasticBeanstalk.DeleteApplication
    (
    -- * Request
      DeleteApplication
    -- ** Request constructor
    , deleteApplication
    -- ** Request lenses
    , daApplicationName
    , daTerminateEnvByForce

    -- * Response
    , DeleteApplicationResponse
    -- ** Response constructor
    , deleteApplicationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DeleteApplication = DeleteApplication
    { _daApplicationName :: Text
    , _daTerminateEnvByForce :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApplication' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @TerminateEnvByForce ::@ @Maybe Bool@
--
deleteApplication :: Text -- ^ 'daApplicationName'
                  -> DeleteApplication
deleteApplication p1 = DeleteApplication
    { _daApplicationName = p1
    , _daTerminateEnvByForce = Nothing
    }

-- | The name of the application to delete.
daApplicationName :: Lens' DeleteApplication Text
daApplicationName =
    lens _daApplicationName (\s a -> s { _daApplicationName = a })

-- | When set to true, running environments will be terminated before deleting
-- the application.
daTerminateEnvByForce :: Lens' DeleteApplication (Maybe Bool)
daTerminateEnvByForce =
    lens _daTerminateEnvByForce (\s a -> s { _daTerminateEnvByForce = a })

instance ToQuery DeleteApplication where
    toQuery = genericQuery def

data DeleteApplicationResponse = DeleteApplicationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApplicationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteApplicationResponse :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse

instance AWSRequest DeleteApplication where
    type Sv DeleteApplication = ElasticBeanstalk
    type Rs DeleteApplication = DeleteApplicationResponse

    request = post "DeleteApplication"
    response _ = nullaryResponse DeleteApplicationResponse
