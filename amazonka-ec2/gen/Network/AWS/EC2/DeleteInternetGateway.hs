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

-- Module      : Network.AWS.EC2.DeleteInternetGateway
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

-- | Deletes the specified Internet gateway. You must detach the Internet gateway
-- from the VPC before you can delete it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteInternetGateway.html>
module Network.AWS.EC2.DeleteInternetGateway
    (
    -- * Request
      DeleteInternetGateway
    -- ** Request constructor
    , deleteInternetGateway
    -- ** Request lenses
    , dig2DryRun
    , dig2InternetGatewayId

    -- * Response
    , DeleteInternetGatewayResponse
    -- ** Response constructor
    , deleteInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteInternetGateway = DeleteInternetGateway
    { _dig2DryRun            :: Maybe Bool
    , _dig2InternetGatewayId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dig2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dig2InternetGatewayId' @::@ 'Text'
--
deleteInternetGateway :: Text -- ^ 'dig2InternetGatewayId'
                      -> DeleteInternetGateway
deleteInternetGateway p1 = DeleteInternetGateway
    { _dig2InternetGatewayId = p1
    , _dig2DryRun            = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dig2DryRun :: Lens' DeleteInternetGateway (Maybe Bool)
dig2DryRun = lens _dig2DryRun (\s a -> s { _dig2DryRun = a })

-- | The ID of the Internet gateway.
dig2InternetGatewayId :: Lens' DeleteInternetGateway Text
dig2InternetGatewayId =
    lens _dig2InternetGatewayId (\s a -> s { _dig2InternetGatewayId = a })

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteInternetGatewayResponse' constructor.
deleteInternetGatewayResponse :: DeleteInternetGatewayResponse
deleteInternetGatewayResponse = DeleteInternetGatewayResponse

instance ToPath DeleteInternetGateway where
    toPath = const "/"

instance ToQuery DeleteInternetGateway where
    toQuery DeleteInternetGateway{..} = mconcat
        [ "DryRun"            =? _dig2DryRun
        , "InternetGatewayId" =? _dig2InternetGatewayId
        ]

instance ToHeaders DeleteInternetGateway

instance AWSRequest DeleteInternetGateway where
    type Sv DeleteInternetGateway = EC2
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse

    request  = post "DeleteInternetGateway"
    response = nullResponse DeleteInternetGatewayResponse
