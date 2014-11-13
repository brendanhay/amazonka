{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.CreateAccessKey
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
-- access keys under the AWS account, you can use this action to manage root
-- credentials even if the AWS account has no associated users. For
-- information about limits on the number of keys you can create, see
-- Limitations on IAM Entities in the Using IAM guide. To ensure the security
-- of your AWS account, the secret access key is accessible only during key
-- and user creation. You must save the key (for example, in a text file) if
-- you want to be able to access it again. If a secret key is lost, you can
-- delete the access keys for the associated user and then create new keys.
module Network.AWS.IAM.CreateAccessKey
    (
    -- * Request
      CreateAccessKey
    -- ** Request constructor
    , createAccessKey
    -- ** Request lenses
    , cakUserName

    -- * Response
    , CreateAccessKeyResponse
    -- ** Response constructor
    , createAccessKeyResponse
    -- ** Response lenses
    , cakrAccessKey
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype CreateAccessKey = CreateAccessKey
    { _cakUserName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateAccessKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cakUserName' @::@ 'Maybe' 'Text'
--
createAccessKey :: CreateAccessKey
createAccessKey = CreateAccessKey
    { _cakUserName = Nothing
    }

-- | The user name that the new key will belong to.
cakUserName :: Lens' CreateAccessKey (Maybe Text)
cakUserName = lens _cakUserName (\s a -> s { _cakUserName = a })

instance ToQuery CreateAccessKey

instance ToPath CreateAccessKey where
    toPath = const "/"

newtype CreateAccessKeyResponse = CreateAccessKeyResponse
    { _cakrAccessKey :: AccessKey
    } deriving (Eq, Show, Generic)

-- | 'CreateAccessKeyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cakrAccessKey' @::@ 'AccessKey'
--
createAccessKeyResponse :: AccessKey -- ^ 'cakrAccessKey'
                        -> CreateAccessKeyResponse
createAccessKeyResponse p1 = CreateAccessKeyResponse
    { _cakrAccessKey = p1
    }

-- | Information about the access key.
cakrAccessKey :: Lens' CreateAccessKeyResponse AccessKey
cakrAccessKey = lens _cakrAccessKey (\s a -> s { _cakrAccessKey = a })

instance AWSRequest CreateAccessKey where
    type Sv CreateAccessKey = IAM
    type Rs CreateAccessKey = CreateAccessKeyResponse

    request  = post "CreateAccessKey"
    response = xmlResponse $ \h x -> CreateAccessKeyResponse
        <$> x %| "AccessKey"
