{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new user for your AWS account. For information about limitations
-- on the number of users you can create, see Limitations on IAM Entities in
-- the Using IAM guide. https://iam.amazonaws.com/ ?Action=CreateUser
-- &Path=/division_abc/subdivision_xyz/ &UserName=Bob &Version=2010-05-08
-- &AUTHPARAMS /division_abc/subdivision_xyz/ Bob AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.CreateUser
    (
    -- * Request
      CreateUser
    -- ** Request constructor
    , mkCreateUserRequest
    -- ** Request lenses
    , curPath
    , curUserName

    -- * Response
    , CreateUserResponse
    -- ** Response lenses
    , cusUser
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateUser' request.
mkCreateUserRequest :: Text -- ^ 'curUserName'
                    -> CreateUser
mkCreateUserRequest p1 = CreateUser
    { _curPath = Nothing
    , _curUserName = p2
    }
{-# INLINE mkCreateUserRequest #-}

data CreateUser = CreateUser
    { _curPath :: Maybe Text
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/).
    , _curUserName :: Text
      -- ^ Name of the user to create.
    } deriving (Show, Generic)

-- | The path for the user name. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide. This parameter is
-- optional. If it is not included, it defaults to a slash (/).
curPath :: Lens' CreateUser (Maybe Text)
curPath = lens _curPath (\s a -> s { _curPath = a })
{-# INLINE curPath #-}

-- | Name of the user to create.
curUserName :: Lens' CreateUser (Text)
curUserName = lens _curUserName (\s a -> s { _curUserName = a })
{-# INLINE curUserName #-}

instance ToQuery CreateUser where
    toQuery = genericQuery def

newtype CreateUserResponse = CreateUserResponse
    { _cusUser :: Maybe User
      -- ^ Information about the user.
    } deriving (Show, Generic)

-- | Information about the user.
cusUser :: Lens' CreateUserResponse (Maybe User)
cusUser = lens _cusUser (\s a -> s { _cusUser = a })
{-# INLINE cusUser #-}

instance FromXML CreateUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateUser where
    type Sv CreateUser = IAM
    type Rs CreateUser = CreateUserResponse

    request = post "CreateUser"
    response _ = xmlResponse
