{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the access key associated with the specified user. If you do not
-- specify a user name, IAM determines the user name implicitly based on the
-- AWS access key ID signing the request. Because this action works for access
-- keys under the AWS account, you can use this API to manage root credentials
-- even if the AWS account has no associated users. https://iam.amazonaws.com/
-- ?Action=DeleteAccessKey &UserName=Bob &AccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteAccessKey
    (
    -- * Request
      DeleteAccessKey
    -- ** Request constructor
    , deleteAccessKey
    -- ** Request lenses
    , dakrAccessKeyId
    , dakrUserName

    -- * Response
    , DeleteAccessKeyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteAccessKey' request.
deleteAccessKey :: Text -- ^ 'dakrAccessKeyId'
                -> DeleteAccessKey
deleteAccessKey p1 = DeleteAccessKey
    { _dakrAccessKeyId = p1
    , _dakrUserName = Nothing
    }
{-# INLINE deleteAccessKey #-}

data DeleteAccessKey = DeleteAccessKey
    { _dakrAccessKeyId :: Text
      -- ^ The access key ID for the access key ID and secret access key you
      -- want to delete.
    , _dakrUserName :: Maybe Text
      -- ^ Name of the user whose key you want to delete.
    } deriving (Show, Generic)

-- | The access key ID for the access key ID and secret access key you want to
-- delete.
dakrAccessKeyId :: Lens' DeleteAccessKey (Text)
dakrAccessKeyId f x =
    f (_dakrAccessKeyId x)
        <&> \y -> x { _dakrAccessKeyId = y }
{-# INLINE dakrAccessKeyId #-}

-- | Name of the user whose key you want to delete.
dakrUserName :: Lens' DeleteAccessKey (Maybe Text)
dakrUserName f x =
    f (_dakrUserName x)
        <&> \y -> x { _dakrUserName = y }
{-# INLINE dakrUserName #-}

instance ToQuery DeleteAccessKey where
    toQuery = genericQuery def

data DeleteAccessKeyResponse = DeleteAccessKeyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteAccessKey where
    type Sv DeleteAccessKey = IAM
    type Rs DeleteAccessKey = DeleteAccessKeyResponse

    request = post "DeleteAccessKey"
    response _ = nullaryResponse DeleteAccessKeyResponse
