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

-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation updates the type of medium changer in a gateway-VTL. When you
-- activate a gateway-VTL, you select a medium changer type for the gateway-VTL.
-- This operation enables you to select a different type of medium changer after
-- a gateway-VTL is activated.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateVTLDeviceType.html>
module Network.AWS.StorageGateway.UpdateVTLDeviceType
    (
    -- * Request
      UpdateVTLDeviceType
    -- ** Request constructor
    , updateVTLDeviceType
    -- ** Request lenses
    , uvtldtDeviceType
    , uvtldtVTLDeviceARN

    -- * Response
    , UpdateVTLDeviceTypeResponse
    -- ** Response constructor
    , updateVTLDeviceTypeResponse
    -- ** Response lenses
    , uvtldtrVTLDeviceARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data UpdateVTLDeviceType = UpdateVTLDeviceType
    { _uvtldtDeviceType   :: Text
    , _uvtldtVTLDeviceARN :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateVTLDeviceType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtDeviceType' @::@ 'Text'
--
-- * 'uvtldtVTLDeviceARN' @::@ 'Text'
--
updateVTLDeviceType :: Text -- ^ 'uvtldtVTLDeviceARN'
                    -> Text -- ^ 'uvtldtDeviceType'
                    -> UpdateVTLDeviceType
updateVTLDeviceType p1 p2 = UpdateVTLDeviceType
    { _uvtldtVTLDeviceARN = p1
    , _uvtldtDeviceType   = p2
    }

-- | The type of medium changer you want to select.
--
-- /Valid Values/: "STK-L700", "AWS-Gateway-VTL"
uvtldtDeviceType :: Lens' UpdateVTLDeviceType Text
uvtldtDeviceType = lens _uvtldtDeviceType (\s a -> s { _uvtldtDeviceType = a })

-- | The Amazon Resource Name (ARN) of the medium changer you want to select.
uvtldtVTLDeviceARN :: Lens' UpdateVTLDeviceType Text
uvtldtVTLDeviceARN =
    lens _uvtldtVTLDeviceARN (\s a -> s { _uvtldtVTLDeviceARN = a })

newtype UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse
    { _uvtldtrVTLDeviceARN :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UpdateVTLDeviceTypeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtrVTLDeviceARN' @::@ 'Maybe' 'Text'
--
updateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse
updateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse
    { _uvtldtrVTLDeviceARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
uvtldtrVTLDeviceARN :: Lens' UpdateVTLDeviceTypeResponse (Maybe Text)
uvtldtrVTLDeviceARN =
    lens _uvtldtrVTLDeviceARN (\s a -> s { _uvtldtrVTLDeviceARN = a })

instance ToPath UpdateVTLDeviceType where
    toPath = const "/"

instance ToQuery UpdateVTLDeviceType where
    toQuery = const mempty

instance ToHeaders UpdateVTLDeviceType

instance ToJSON UpdateVTLDeviceType where
    toJSON UpdateVTLDeviceType{..} = object
        [ "VTLDeviceARN" .= _uvtldtVTLDeviceARN
        , "DeviceType"   .= _uvtldtDeviceType
        ]

instance AWSRequest UpdateVTLDeviceType where
    type Sv UpdateVTLDeviceType = StorageGateway
    type Rs UpdateVTLDeviceType = UpdateVTLDeviceTypeResponse

    request  = post "UpdateVTLDeviceType"
    response = jsonResponse

instance FromJSON UpdateVTLDeviceTypeResponse where
    parseJSON = withObject "UpdateVTLDeviceTypeResponse" $ \o -> UpdateVTLDeviceTypeResponse
        <$> o .:? "VTLDeviceARN"
