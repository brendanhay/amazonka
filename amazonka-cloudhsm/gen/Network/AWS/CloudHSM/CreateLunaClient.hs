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

-- Module      : Network.AWS.CloudHSM.CreateLunaClient
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

-- | Creates an HSM client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateLunaClient.html>
module Network.AWS.CloudHSM.CreateLunaClient
    (
    -- * Request
      CreateLunaClient
    -- ** Request constructor
    , createLunaClient
    -- ** Request lenses
    , clcCertificate
    , clcLabel

    -- * Response
    , CreateLunaClientResponse
    -- ** Response constructor
    , createLunaClientResponse
    -- ** Response lenses
    , clcrClientArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data CreateLunaClient = CreateLunaClient
    { _clcCertificate :: Text
    , _clcLabel       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateLunaClient' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcCertificate' @::@ 'Text'
--
-- * 'clcLabel' @::@ 'Maybe' 'Text'
--
createLunaClient :: Text -- ^ 'clcCertificate'
                 -> CreateLunaClient
createLunaClient p1 = CreateLunaClient
    { _clcCertificate = p1
    , _clcLabel       = Nothing
    }

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the
-- HSMs used by this client.
clcCertificate :: Lens' CreateLunaClient Text
clcCertificate = lens _clcCertificate (\s a -> s { _clcCertificate = a })

-- | The label for the client.
clcLabel :: Lens' CreateLunaClient (Maybe Text)
clcLabel = lens _clcLabel (\s a -> s { _clcLabel = a })

newtype CreateLunaClientResponse = CreateLunaClientResponse
    { _clcrClientArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateLunaClientResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clcrClientArn' @::@ 'Maybe' 'Text'
--
createLunaClientResponse :: CreateLunaClientResponse
createLunaClientResponse = CreateLunaClientResponse
    { _clcrClientArn = Nothing
    }

-- | The ARN of the client.
clcrClientArn :: Lens' CreateLunaClientResponse (Maybe Text)
clcrClientArn = lens _clcrClientArn (\s a -> s { _clcrClientArn = a })

instance ToPath CreateLunaClient where
    toPath = const "/"

instance ToQuery CreateLunaClient where
    toQuery = const mempty

instance ToHeaders CreateLunaClient

instance ToJSON CreateLunaClient where
    toJSON CreateLunaClient{..} = object
        [ "Label"       .= _clcLabel
        , "Certificate" .= _clcCertificate
        ]

instance AWSRequest CreateLunaClient where
    type Sv CreateLunaClient = CloudHSM
    type Rs CreateLunaClient = CreateLunaClientResponse

    request  = post "CreateLunaClient"
    response = jsonResponse

instance FromJSON CreateLunaClientResponse where
    parseJSON = withObject "CreateLunaClientResponse" $ \o -> CreateLunaClientResponse
        <$> o .:? "ClientArn"
