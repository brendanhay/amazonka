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

-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
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

-- | Modifies the certificate used by the client.
--
-- This action can potentially start a workflow to install the new certificate
-- on the client's HSMs.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyLunaClient.html>
module Network.AWS.CloudHSM.ModifyLunaClient
    (
    -- * Request
      ModifyLunaClient
    -- ** Request constructor
    , modifyLunaClient
    -- ** Request lenses
    , mlcCertificate
    , mlcClientArn

    -- * Response
    , ModifyLunaClientResponse
    -- ** Response constructor
    , modifyLunaClientResponse
    -- ** Response lenses
    , mlcrClientArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data ModifyLunaClient = ModifyLunaClient
    { _mlcCertificate :: Text
    , _mlcClientArn   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyLunaClient' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlcCertificate' @::@ 'Text'
--
-- * 'mlcClientArn' @::@ 'Text'
--
modifyLunaClient :: Text -- ^ 'mlcClientArn'
                 -> Text -- ^ 'mlcCertificate'
                 -> ModifyLunaClient
modifyLunaClient p1 p2 = ModifyLunaClient
    { _mlcClientArn   = p1
    , _mlcCertificate = p2
    }

-- | The new certificate for the client.
mlcCertificate :: Lens' ModifyLunaClient Text
mlcCertificate = lens _mlcCertificate (\s a -> s { _mlcCertificate = a })

-- | The ARN of the client.
mlcClientArn :: Lens' ModifyLunaClient Text
mlcClientArn = lens _mlcClientArn (\s a -> s { _mlcClientArn = a })

newtype ModifyLunaClientResponse = ModifyLunaClientResponse
    { _mlcrClientArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ModifyLunaClientResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlcrClientArn' @::@ 'Maybe' 'Text'
--
modifyLunaClientResponse :: ModifyLunaClientResponse
modifyLunaClientResponse = ModifyLunaClientResponse
    { _mlcrClientArn = Nothing
    }

-- | The ARN of the client.
mlcrClientArn :: Lens' ModifyLunaClientResponse (Maybe Text)
mlcrClientArn = lens _mlcrClientArn (\s a -> s { _mlcrClientArn = a })

instance ToPath ModifyLunaClient where
    toPath = const "/"

instance ToQuery ModifyLunaClient where
    toQuery = const mempty

instance ToHeaders ModifyLunaClient

instance ToJSON ModifyLunaClient where
    toJSON ModifyLunaClient{..} = object
        [ "ClientArn"   .= _mlcClientArn
        , "Certificate" .= _mlcCertificate
        ]

instance AWSRequest ModifyLunaClient where
    type Sv ModifyLunaClient = CloudHSM
    type Rs ModifyLunaClient = ModifyLunaClientResponse

    request  = post "ModifyLunaClient"
    response = jsonResponse

instance FromJSON ModifyLunaClientResponse where
    parseJSON = withObject "ModifyLunaClientResponse" $ \o -> ModifyLunaClientResponse
        <$> o .:? "ClientArn"
