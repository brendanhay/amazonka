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
    , createUser
    -- ** Request lenses
    , curUserName
    , curPath

    -- * Response
    , CreateUserResponse
    -- ** Response lenses
    , cusUser
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateUser' request.
createUser :: Text -- ^ 'curUserName'
           -> CreateUser
createUser p1 = CreateUser
    { _curUserName = p1
    , _curPath = Nothing
    }

data CreateUser = CreateUser
    { _curUserName :: Text
      -- ^ Name of the user to create.
    , _curPath :: Maybe Text
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/).
    } deriving (Show, Generic)

-- | Name of the user to create.
curUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateUser
    -> f CreateUser
curUserName f x =
    (\y -> x { _curUserName = y })
       <$> f (_curUserName x)
{-# INLINE curUserName #-}

-- | The path for the user name. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide. This parameter is
-- optional. If it is not included, it defaults to a slash (/).
curPath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateUser
    -> f CreateUser
curPath f x =
    (\y -> x { _curPath = y })
       <$> f (_curPath x)
{-# INLINE curPath #-}

instance ToQuery CreateUser where
    toQuery = genericQuery def

data CreateUserResponse = CreateUserResponse
    { _cusUser :: Maybe User
      -- ^ Information about the user.
    } deriving (Show, Generic)

-- | Information about the user.
cusUser
    :: Functor f
    => (Maybe User
    -> f (Maybe User))
    -> CreateUserResponse
    -> f CreateUserResponse
cusUser f x =
    (\y -> x { _cusUser = y })
       <$> f (_cusUser x)
{-# INLINE cusUser #-}

instance FromXML CreateUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateUser where
    type Sv CreateUser = IAM
    type Rs CreateUser = CreateUserResponse

    request = post "CreateUser"
    response _ = xmlResponse
