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

-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a specified service within a cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteService.html>
module Network.AWS.ECS.DeleteService
    (
    -- * Request
      DeleteService
    -- ** Request constructor
    , deleteService
    -- ** Request lenses
    , dsCluster
    , dsService

    -- * Response
    , DeleteServiceResponse
    -- ** Response constructor
    , deleteServiceResponse
    -- ** Response lenses
    , dsrService
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data DeleteService = DeleteService
    { _dsCluster :: Maybe Text
    , _dsService :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteService' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsCluster' @::@ 'Maybe' 'Text'
--
-- * 'dsService' @::@ 'Text'
--
deleteService :: Text -- ^ 'dsService'
              -> DeleteService
deleteService p1 = DeleteService
    { _dsService = p1
    , _dsCluster = Nothing
    }

-- | The name of the cluster that hosts the service you want to delete.
dsCluster :: Lens' DeleteService (Maybe Text)
dsCluster = lens _dsCluster (\s a -> s { _dsCluster = a })

-- | The name of the service you want to delete.
dsService :: Lens' DeleteService Text
dsService = lens _dsService (\s a -> s { _dsService = a })

newtype DeleteServiceResponse = DeleteServiceResponse
    { _dsrService :: Maybe ContainerService
    } deriving (Eq, Read, Show)

-- | 'DeleteServiceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrService' @::@ 'Maybe' 'ContainerService'
--
deleteServiceResponse :: DeleteServiceResponse
deleteServiceResponse = DeleteServiceResponse
    { _dsrService = Nothing
    }

dsrService :: Lens' DeleteServiceResponse (Maybe ContainerService)
dsrService = lens _dsrService (\s a -> s { _dsrService = a })

instance ToPath DeleteService where
    toPath = const "/"

instance ToQuery DeleteService where
    toQuery = const mempty

instance ToHeaders DeleteService

instance ToJSON DeleteService where
    toJSON DeleteService{..} = object
        [ "cluster" .= _dsCluster
        , "service" .= _dsService
        ]

instance AWSRequest DeleteService where
    type Sv DeleteService = ECS
    type Rs DeleteService = DeleteServiceResponse

    request  = post "DeleteService"
    response = jsonResponse

instance FromJSON DeleteServiceResponse where
    parseJSON = withObject "DeleteServiceResponse" $ \o -> DeleteServiceResponse
        <$> o .:? "service"
