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

-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
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

-- | This operation updates the Challenge-Handshake Authentication Protocol (CHAP)
-- credentials for a specified iSCSI target. By default, a gateway does not have
-- CHAP enabled; however, for added security, you might use it.
--
-- When you update CHAP credentials, all existing connections on the target
-- are closed and initiators must reconnect with the new credentials.
--
--
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateChapCredentials.html>
module Network.AWS.StorageGateway.UpdateChapCredentials
    (
    -- * Request
      UpdateChapCredentials
    -- ** Request constructor
    , updateChapCredentials
    -- ** Request lenses
    , uccInitiatorName
    , uccSecretToAuthenticateInitiator
    , uccSecretToAuthenticateTarget
    , uccTargetARN

    -- * Response
    , UpdateChapCredentialsResponse
    -- ** Response constructor
    , updateChapCredentialsResponse
    -- ** Response lenses
    , uccrInitiatorName
    , uccrTargetARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data UpdateChapCredentials = UpdateChapCredentials
    { _uccInitiatorName                 :: Text
    , _uccSecretToAuthenticateInitiator :: Text
    , _uccSecretToAuthenticateTarget    :: Maybe Text
    , _uccTargetARN                     :: Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateChapCredentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uccInitiatorName' @::@ 'Text'
--
-- * 'uccSecretToAuthenticateInitiator' @::@ 'Text'
--
-- * 'uccSecretToAuthenticateTarget' @::@ 'Maybe' 'Text'
--
-- * 'uccTargetARN' @::@ 'Text'
--
updateChapCredentials :: Text -- ^ 'uccTargetARN'
                      -> Text -- ^ 'uccSecretToAuthenticateInitiator'
                      -> Text -- ^ 'uccInitiatorName'
                      -> UpdateChapCredentials
updateChapCredentials p1 p2 p3 = UpdateChapCredentials
    { _uccTargetARN                     = p1
    , _uccSecretToAuthenticateInitiator = p2
    , _uccInitiatorName                 = p3
    , _uccSecretToAuthenticateTarget    = Nothing
    }

-- | The iSCSI initiator that connects to the target.
uccInitiatorName :: Lens' UpdateChapCredentials Text
uccInitiatorName = lens _uccInitiatorName (\s a -> s { _uccInitiatorName = a })

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
uccSecretToAuthenticateInitiator :: Lens' UpdateChapCredentials Text
uccSecretToAuthenticateInitiator =
    lens _uccSecretToAuthenticateInitiator
        (\s a -> s { _uccSecretToAuthenticateInitiator = a })

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
uccSecretToAuthenticateTarget :: Lens' UpdateChapCredentials (Maybe Text)
uccSecretToAuthenticateTarget =
    lens _uccSecretToAuthenticateTarget
        (\s a -> s { _uccSecretToAuthenticateTarget = a })

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the 'DescribeStorediSCSIVolumes' operation to return to retrieve the TargetARN for specified VolumeARN.
uccTargetARN :: Lens' UpdateChapCredentials Text
uccTargetARN = lens _uccTargetARN (\s a -> s { _uccTargetARN = a })

data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse
    { _uccrInitiatorName :: Maybe Text
    , _uccrTargetARN     :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateChapCredentialsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uccrInitiatorName' @::@ 'Maybe' 'Text'
--
-- * 'uccrTargetARN' @::@ 'Maybe' 'Text'
--
updateChapCredentialsResponse :: UpdateChapCredentialsResponse
updateChapCredentialsResponse = UpdateChapCredentialsResponse
    { _uccrTargetARN     = Nothing
    , _uccrInitiatorName = Nothing
    }

-- | The iSCSI initiator that connects to the target. This is the same initiator
-- name specified in the request.
uccrInitiatorName :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrInitiatorName =
    lens _uccrInitiatorName (\s a -> s { _uccrInitiatorName = a })

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
uccrTargetARN :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrTargetARN = lens _uccrTargetARN (\s a -> s { _uccrTargetARN = a })

instance ToPath UpdateChapCredentials where
    toPath = const "/"

instance ToQuery UpdateChapCredentials where
    toQuery = const mempty

instance ToHeaders UpdateChapCredentials

instance ToJSON UpdateChapCredentials where
    toJSON UpdateChapCredentials{..} = object
        [ "TargetARN"                     .= _uccTargetARN
        , "SecretToAuthenticateInitiator" .= _uccSecretToAuthenticateInitiator
        , "InitiatorName"                 .= _uccInitiatorName
        , "SecretToAuthenticateTarget"    .= _uccSecretToAuthenticateTarget
        ]

instance AWSRequest UpdateChapCredentials where
    type Sv UpdateChapCredentials = StorageGateway
    type Rs UpdateChapCredentials = UpdateChapCredentialsResponse

    request  = post "UpdateChapCredentials"
    response = jsonResponse

instance FromJSON UpdateChapCredentialsResponse where
    parseJSON = withObject "UpdateChapCredentialsResponse" $ \o -> UpdateChapCredentialsResponse
        <$> o .:? "InitiatorName"
        <*> o .:? "TargetARN"
