{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new AWS secret access key and corresponding AWS access key ID for
-- the specified user. The default status for new keys is Active. If you do
-- not specify a user name, IAM determines the user name implicitly based on
-- the AWS access key ID signing the request. Because this action works for
-- access keys under the AWS account, you can use this API to manage root
-- credentials even if the AWS account has no associated users. For
-- information about limits on the number of keys you can create, see
-- Limitations on IAM Entities in the Using IAM guide. To ensure the security
-- of your AWS account, the secret access key is accessible only during key
-- and user creation. You must save the key (for example, in a text file) if
-- you want to be able to access it again. If a secret key is lost, you can
-- delete the access keys for the associated user and then create new keys.
-- https://iam.amazonaws.com/ ?Action=CreateAccessKey &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS Bob AKIAIOSFODNN7EXAMPLE Active
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.CreateAccessKey
    (
    -- * Request
      CreateAccessKey
    -- ** Request constructor
    , mkCreateAccessKey
    -- ** Request lenses
    , cakUserName

    -- * Response
    , CreateAccessKeyResponse
    -- ** Response lenses
    , cakrsAccessKey
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
newtype CreateAccessKey = CreateAccessKey
    { _cakUserName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateAccessKey' request.
mkCreateAccessKey :: CreateAccessKey
mkCreateAccessKey = CreateAccessKey
    { _cakUserName = Nothing
    }
{-# INLINE mkCreateAccessKey #-}

-- | The user name that the new key will belong to.
cakUserName :: Lens' CreateAccessKey (Maybe Text)
cakUserName = lens _cakUserName (\s a -> s { _cakUserName = a })
{-# INLINE cakUserName #-}

instance ToQuery CreateAccessKey where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the CreateAccessKey
-- action.
newtype CreateAccessKeyResponse = CreateAccessKeyResponse
    { _cakrsAccessKey :: AccessKey
    } deriving (Show, Generic)

-- | Information about the access key.
cakrsAccessKey :: Lens' CreateAccessKeyResponse AccessKey
cakrsAccessKey = lens _cakrsAccessKey (\s a -> s { _cakrsAccessKey = a })
{-# INLINE cakrsAccessKey #-}

instance FromXML CreateAccessKeyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateAccessKey where
    type Sv CreateAccessKey = IAM
    type Rs CreateAccessKey = CreateAccessKeyResponse

    request = post "CreateAccessKey"
    response _ = xmlResponse
