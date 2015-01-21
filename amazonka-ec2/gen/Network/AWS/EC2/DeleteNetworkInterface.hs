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

-- Module      : Network.AWS.EC2.DeleteNetworkInterface
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

-- | Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkInterface.html>
module Network.AWS.EC2.DeleteNetworkInterface
    (
    -- * Request
      DeleteNetworkInterface
    -- ** Request constructor
    , deleteNetworkInterface
    -- ** Request lenses
    , dni2DryRun
    , dni2NetworkInterfaceId

    -- * Response
    , DeleteNetworkInterfaceResponse
    -- ** Response constructor
    , deleteNetworkInterfaceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteNetworkInterface = DeleteNetworkInterface
    { _dni2DryRun             :: Maybe Bool
    , _dni2NetworkInterfaceId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dni2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dni2NetworkInterfaceId' @::@ 'Text'
--
deleteNetworkInterface :: Text -- ^ 'dni2NetworkInterfaceId'
                       -> DeleteNetworkInterface
deleteNetworkInterface p1 = DeleteNetworkInterface
    { _dni2NetworkInterfaceId = p1
    , _dni2DryRun             = Nothing
    }

dni2DryRun :: Lens' DeleteNetworkInterface (Maybe Bool)
dni2DryRun = lens _dni2DryRun (\s a -> s { _dni2DryRun = a })

-- | The ID of the network interface.
dni2NetworkInterfaceId :: Lens' DeleteNetworkInterface Text
dni2NetworkInterfaceId =
    lens _dni2NetworkInterfaceId (\s a -> s { _dni2NetworkInterfaceId = a })

data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteNetworkInterfaceResponse' constructor.
deleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse
deleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse

instance ToPath DeleteNetworkInterface where
    toPath = const "/"

instance ToQuery DeleteNetworkInterface where
    toQuery DeleteNetworkInterface{..} = mconcat
        [ "dryRun"             =? _dni2DryRun
        , "networkInterfaceId" =? _dni2NetworkInterfaceId
        ]

instance ToHeaders DeleteNetworkInterface

instance AWSRequest DeleteNetworkInterface where
    type Sv DeleteNetworkInterface = EC2
    type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse

    request  = post "DeleteNetworkInterface"
    response = nullResponse DeleteNetworkInterfaceResponse
