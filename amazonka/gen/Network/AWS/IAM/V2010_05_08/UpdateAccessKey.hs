{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.IAM.V2010_05_08.UpdateAccessKey where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateAccessKey' request.
updateAccessKey :: Text -- ^ '_uakrAccessKeyId'
                -> StatusType -- ^ '_uakrStatus'
                -> UpdateAccessKey
updateAccessKey p1 p2 = UpdateAccessKey
    { _uakrAccessKeyId = p1
    , _uakrStatus = p2
    , _uakrUserName = Nothing
    }

data UpdateAccessKey = UpdateAccessKey
    { _uakrAccessKeyId :: Text
      -- ^ The access key ID of the secret access key you want to update.
    , _uakrStatus :: StatusType
      -- ^ The status you want to assign to the secret access key. Active
      -- means the key can be used for API calls to AWS, while Inactive
      -- means the key cannot be used.
    , _uakrUserName :: Maybe Text
      -- ^ Name of the user whose key you want to update.
    } deriving (Generic)

makeLenses ''UpdateAccessKey

instance ToQuery UpdateAccessKey where
    toQuery = genericToQuery def

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateAccessKeyResponse

instance AWSRequest UpdateAccessKey where
    type Sv UpdateAccessKey = IAM
    type Rs UpdateAccessKey = UpdateAccessKeyResponse

    request = post "UpdateAccessKey"
    response _ _ = return (Right UpdateAccessKeyResponse)
