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
-- Amazon S3 bucket.
module Network.AWS.ElasticBeanstalk.DeleteApplication
    (
    -- * Request
      DeleteApplicationMessage
    -- ** Request constructor
    , deleteApplicationMessage
    -- ** Request lenses
    , damApplicationName
    , damTerminateEnvByForce

    -- * Response
    , DeleteApplicationResponse
    -- ** Response constructor
    , deleteApplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DeleteApplicationMessage = DeleteApplicationMessage
    { _damApplicationName     :: Text
    , _damTerminateEnvByForce :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteApplicationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damApplicationName' @::@ 'Text'
--
-- * 'damTerminateEnvByForce' @::@ 'Maybe' 'Bool'
--
deleteApplicationMessage :: Text -- ^ 'damApplicationName'
                         -> DeleteApplicationMessage
deleteApplicationMessage p1 = DeleteApplicationMessage
    { _damApplicationName     = p1
    , _damTerminateEnvByForce = Nothing
    }

-- | The name of the application to delete.
damApplicationName :: Lens' DeleteApplicationMessage Text
damApplicationName =
    lens _damApplicationName (\s a -> s { _damApplicationName = a })

-- | When set to true, running environments will be terminated before deleting
-- the application.
damTerminateEnvByForce :: Lens' DeleteApplicationMessage (Maybe Bool)
damTerminateEnvByForce =
    lens _damTerminateEnvByForce (\s a -> s { _damTerminateEnvByForce = a })

instance ToPath DeleteApplicationMessage where
    toPath = const "/"

instance ToQuery DeleteApplicationMessage

data DeleteApplicationResponse = DeleteApplicationResponse

-- | 'DeleteApplicationResponse' constructor.
deleteApplicationResponse :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse

instance AWSRequest DeleteApplicationMessage where
    type Sv DeleteApplicationMessage = ElasticBeanstalk
    type Rs DeleteApplicationMessage = DeleteApplicationResponse

    request  = post "DeleteApplication"
    response = const (nullaryResponse DeleteApplicationResponse)
