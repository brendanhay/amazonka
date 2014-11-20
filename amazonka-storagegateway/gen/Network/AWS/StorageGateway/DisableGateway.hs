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

-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables a gateway when the gateway is no longer functioning. For example,
-- if your gateway VM is damaged, you can disable the gateway so you can
-- recover virtual tapes. Use this operation for a gateway-VTL that is not
-- reachable or not functioning. Once a gateway is disabled it cannot be
-- enabled.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DisableGateway.html>
module Network.AWS.StorageGateway.DisableGateway
    (
    -- * Request
      DisableGateway
    -- ** Request constructor
    , disableGateway
    -- ** Request lenses
    , dg1GatewayARN

    -- * Response
    , DisableGatewayResponse
    -- ** Response constructor
    , disableGatewayResponse
    -- ** Response lenses
    , dgr1GatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DisableGateway = DisableGateway
    { _dg1GatewayARN :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DisableGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dg1GatewayARN' @::@ 'Text'
--
disableGateway :: Text -- ^ 'dg1GatewayARN'
               -> DisableGateway
disableGateway p1 = DisableGateway
    { _dg1GatewayARN = p1
    }

dg1GatewayARN :: Lens' DisableGateway Text
dg1GatewayARN = lens _dg1GatewayARN (\s a -> s { _dg1GatewayARN = a })

newtype DisableGatewayResponse = DisableGatewayResponse
    { _dgr1GatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DisableGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgr1GatewayARN' @::@ 'Maybe' 'Text'
--
disableGatewayResponse :: DisableGatewayResponse
disableGatewayResponse = DisableGatewayResponse
    { _dgr1GatewayARN = Nothing
    }

-- | The unique Amazon Resource Name of the disabled gateway.
dgr1GatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
dgr1GatewayARN = lens _dgr1GatewayARN (\s a -> s { _dgr1GatewayARN = a })

instance ToPath DisableGateway where
    toPath = const "/"

instance ToQuery DisableGateway where
    toQuery = const mempty

instance ToHeaders DisableGateway

instance ToJSON DisableGateway where
    toJSON DisableGateway{..} = object
        [ "GatewayARN" .= _dg1GatewayARN
        ]

instance AWSRequest DisableGateway where
    type Sv DisableGateway = StorageGateway
    type Rs DisableGateway = DisableGatewayResponse

    request  = post "DisableGateway"
    response = jsonResponse

instance FromJSON DisableGatewayResponse where
    parseJSON = withObject "DisableGatewayResponse" $ \o -> DisableGatewayResponse
        <$> o .:? "GatewayARN"


Some kind of operator / class to check the types whether to continue?
