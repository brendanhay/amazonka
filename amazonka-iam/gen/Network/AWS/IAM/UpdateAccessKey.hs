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

-- Module      : Network.AWS.IAM.UpdateAccessKey
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

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a key
-- rotation work flow.
--
-- If the 'UserName' field is not specified, the UserName is determined
-- implicitly based on the AWS access key ID used to sign the request. Because
-- this action works for access keys under the AWS account, you can use this
-- action to manage root credentials even if the AWS account has no associated
-- users.
--
-- For information about rotating keys, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing Keys and Certificates> in
-- the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccessKey.html>
module Network.AWS.IAM.UpdateAccessKey
    (
    -- * Request
      UpdateAccessKey
    -- ** Request constructor
    , updateAccessKey
    -- ** Request lenses
    , uakAccessKeyId
    , uakStatus
    , uakUserName

    -- * Response
    , UpdateAccessKeyResponse
    -- ** Response constructor
    , updateAccessKeyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateAccessKey = UpdateAccessKey
    { _uakAccessKeyId :: Text
    , _uakStatus      :: StatusType
    , _uakUserName    :: Maybe Text
    } deriving (Eq, Show)

-- | 'UpdateAccessKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uakAccessKeyId' @::@ 'Text'
--
-- * 'uakStatus' @::@ 'StatusType'
--
-- * 'uakUserName' @::@ 'Maybe' 'Text'
--
updateAccessKey :: Text -- ^ 'uakAccessKeyId'
                -> StatusType -- ^ 'uakStatus'
                -> UpdateAccessKey
updateAccessKey p1 p2 = UpdateAccessKey
    { _uakAccessKeyId = p1
    , _uakStatus      = p2
    , _uakUserName    = Nothing
    }

-- | The access key ID of the secret access key you want to update.
uakAccessKeyId :: Lens' UpdateAccessKey Text
uakAccessKeyId = lens _uakAccessKeyId (\s a -> s { _uakAccessKeyId = a })

-- | The status you want to assign to the secret access key. 'Active' means the key
-- can be used for API calls to AWS, while 'Inactive' means the key cannot be
-- used.
uakStatus :: Lens' UpdateAccessKey StatusType
uakStatus = lens _uakStatus (\s a -> s { _uakStatus = a })

-- | The name of the user whose key you want to update.
uakUserName :: Lens' UpdateAccessKey (Maybe Text)
uakUserName = lens _uakUserName (\s a -> s { _uakUserName = a })

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAccessKeyResponse' constructor.
updateAccessKeyResponse :: UpdateAccessKeyResponse
updateAccessKeyResponse = UpdateAccessKeyResponse

instance ToPath UpdateAccessKey where
    toPath = const "/"

instance ToQuery UpdateAccessKey where
    toQuery UpdateAccessKey{..} = mconcat
        [ "AccessKeyId" =? _uakAccessKeyId
        , "Status"      =? _uakStatus
        , "UserName"    =? _uakUserName
        ]

instance ToHeaders UpdateAccessKey

instance AWSRequest UpdateAccessKey where
    type Sv UpdateAccessKey = IAM
    type Rs UpdateAccessKey = UpdateAccessKeyResponse

    request  = post "UpdateAccessKey"
    response = nullResponse UpdateAccessKeyResponse
