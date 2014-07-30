{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.IAM.V2010_05_08.CreateAccessKey where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'CreateAccessKey' request.
createAccessKey :: CreateAccessKey
createAccessKey = CreateAccessKey
    { _cakrUserName = Nothing
    }

data CreateAccessKey = CreateAccessKey
    { _cakrUserName :: Maybe Text
      -- ^ The user name that the new key will belong to.
    } deriving (Generic)

instance ToQuery CreateAccessKey where
    toQuery = genericToQuery def

instance AWSRequest CreateAccessKey where
    type Sv CreateAccessKey = IAM
    type Rs CreateAccessKey = CreateAccessKeyResponse

    request = post "CreateAccessKey"
    response _ = xmlResponse

data CreateAccessKeyResponse = CreateAccessKeyResponse
    { _caksAccessKey :: AccessKey
      -- ^ Information about the access key.
    } deriving (Generic)

instance FromXML CreateAccessKeyResponse where
    fromXMLOptions = xmlOptions
