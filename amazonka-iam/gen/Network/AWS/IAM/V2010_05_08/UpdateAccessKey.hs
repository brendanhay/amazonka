{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a
-- key rotation work flow. If the UserName field is not specified, the
-- UserName is determined implicitly based on the AWS access key ID used to
-- sign the request. Because this action works for access keys under the AWS
-- account, this API can be used to manage root credentials even if the AWS
-- account has no associated users. For information about rotating keys, see
-- Managing Keys and Certificates in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=UpdateAccessKey &UserName=Bob
-- &AccessKeyId=AKIAIOSFODNN7EXAMPLE &Status=Inactive &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateAccessKey
    (
    -- * Request
      UpdateAccessKey
    -- ** Request constructor
    , mkUpdateAccessKeyRequest
    -- ** Request lenses
    , uakrUserName
    , uakrAccessKeyId
    , uakrStatus

    -- * Response
    , UpdateAccessKeyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccessKey' request.
mkUpdateAccessKeyRequest :: Text -- ^ 'uakrAccessKeyId'
                         -> StatusType -- ^ 'uakrStatus'
                         -> UpdateAccessKey
mkUpdateAccessKeyRequest p1 p2 = UpdateAccessKey
    { _uakrUserName = Nothing
    , _uakrAccessKeyId = p2
    , _uakrStatus = p3
    }
{-# INLINE mkUpdateAccessKeyRequest #-}

data UpdateAccessKey = UpdateAccessKey
    { _uakrUserName :: Maybe Text
      -- ^ Name of the user whose key you want to update.
    , _uakrAccessKeyId :: Text
      -- ^ The access key ID of the secret access key you want to update.
    , _uakrStatus :: StatusType
      -- ^ The status you want to assign to the secret access key. Active
      -- means the key can be used for API calls to AWS, while Inactive
      -- means the key cannot be used.
    } deriving (Show, Generic)

-- | Name of the user whose key you want to update.
uakrUserName :: Lens' UpdateAccessKey (Maybe Text)
uakrUserName = lens _uakrUserName (\s a -> s { _uakrUserName = a })
{-# INLINE uakrUserName #-}

-- | The access key ID of the secret access key you want to update.
uakrAccessKeyId :: Lens' UpdateAccessKey (Text)
uakrAccessKeyId = lens _uakrAccessKeyId (\s a -> s { _uakrAccessKeyId = a })
{-# INLINE uakrAccessKeyId #-}

-- | The status you want to assign to the secret access key. Active means the
-- key can be used for API calls to AWS, while Inactive means the key cannot
-- be used.
uakrStatus :: Lens' UpdateAccessKey (StatusType)
uakrStatus = lens _uakrStatus (\s a -> s { _uakrStatus = a })
{-# INLINE uakrStatus #-}

instance ToQuery UpdateAccessKey where
    toQuery = genericQuery def

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAccessKey where
    type Sv UpdateAccessKey = IAM
    type Rs UpdateAccessKey = UpdateAccessKeyResponse

    request = post "UpdateAccessKey"
    response _ = nullaryResponse UpdateAccessKeyResponse
