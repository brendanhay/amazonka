{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates a gateway's metadata, which includes the gateway's
-- name and time zone. To specify which gateway to update, use the Amazon
-- Resource Name (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewayInformation.html>
module Network.AWS.StorageGateway.UpdateGatewayInformation
    (
    -- * Request
      UpdateGatewayInformation
    -- ** Request constructor
    , updateGatewayInformation
    -- ** Request lenses
    , ugiGatewayARN
    , ugiGatewayName
    , ugiGatewayTimezone

    -- * Response
    , UpdateGatewayInformationResponse
    -- ** Response constructor
    , updateGatewayInformationResponse
    -- ** Response lenses
    , ugirGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data UpdateGatewayInformation = UpdateGatewayInformation
    { _ugiGatewayARN      :: Text
    , _ugiGatewayName     :: Maybe Text
    , _ugiGatewayTimezone :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateGatewayInformation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugiGatewayARN' @::@ 'Text'
--
-- * 'ugiGatewayName' @::@ 'Maybe' 'Text'
--
-- * 'ugiGatewayTimezone' @::@ 'Maybe' 'Text'
--
updateGatewayInformation :: Text -- ^ 'ugiGatewayARN'
                         -> UpdateGatewayInformation
updateGatewayInformation p1 = UpdateGatewayInformation
    { _ugiGatewayARN      = p1
    , _ugiGatewayName     = Nothing
    , _ugiGatewayTimezone = Nothing
    }

ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\s a -> s { _ugiGatewayARN = a })

ugiGatewayName :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayName = lens _ugiGatewayName (\s a -> s { _ugiGatewayName = a })

ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone =
    lens _ugiGatewayTimezone (\s a -> s { _ugiGatewayTimezone = a })

newtype UpdateGatewayInformationResponse = UpdateGatewayInformationResponse
    { _ugirGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UpdateGatewayInformationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugirGatewayARN' @::@ 'Maybe' 'Text'
--
updateGatewayInformationResponse :: UpdateGatewayInformationResponse
updateGatewayInformationResponse = UpdateGatewayInformationResponse
    { _ugirGatewayARN = Nothing
    }

ugirGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirGatewayARN = lens _ugirGatewayARN (\s a -> s { _ugirGatewayARN = a })

instance ToPath UpdateGatewayInformation where
    toPath = const "/"

instance ToQuery UpdateGatewayInformation where
    toQuery = const mempty

instance ToHeaders UpdateGatewayInformation

instance ToJSON UpdateGatewayInformation where
    toJSON UpdateGatewayInformation{..} = object
        [ "GatewayARN"      .= _ugiGatewayARN
        , "GatewayName"     .= _ugiGatewayName
        , "GatewayTimezone" .= _ugiGatewayTimezone
        ]

instance AWSRequest UpdateGatewayInformation where
    type Sv UpdateGatewayInformation = StorageGateway
    type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse

    request  = post "UpdateGatewayInformation"
    response = jsonResponse

instance FromJSON UpdateGatewayInformationResponse where
    parseJSON = withObject "UpdateGatewayInformationResponse" $ \o -> UpdateGatewayInformationResponse
        <$> o .: "GatewayARN"
