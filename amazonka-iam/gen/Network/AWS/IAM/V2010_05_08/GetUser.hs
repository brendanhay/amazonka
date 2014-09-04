{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified user, including the user's path,
-- unique ID, and ARN. If you do not specify a user name, IAM determines the
-- user name implicitly based on the AWS access key ID signing the request.
-- https://iam.amazonaws.com/ ?Action=GetUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.GetUser
    (
    -- * Request
      GetUser
    -- ** Request constructor
    , mkGetUserRequest
    -- ** Request lenses
    , gurUserName

    -- * Response
    , GetUserResponse
    -- ** Response lenses
    , gusUser
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetUser' request.
mkGetUserRequest :: GetUser
mkGetUserRequest = GetUser
    { _gurUserName = Nothing
    }
{-# INLINE mkGetUserRequest #-}

newtype GetUser = GetUser
    { _gurUserName :: Maybe Text
      -- ^ Name of the user to get information about. This parameter is
      -- optional. If it is not included, it defaults to the user making
      -- the request.
    } deriving (Show, Generic)

-- | Name of the user to get information about. This parameter is optional. If
-- it is not included, it defaults to the user making the request.
gurUserName :: Lens' GetUser (Maybe Text)
gurUserName = lens _gurUserName (\s a -> s { _gurUserName = a })
{-# INLINE gurUserName #-}

instance ToQuery GetUser where
    toQuery = genericQuery def

newtype GetUserResponse = GetUserResponse
    { _gusUser :: User
      -- ^ Information about the user.
    } deriving (Show, Generic)

-- | Information about the user.
gusUser :: Lens' GetUserResponse (User)
gusUser = lens _gusUser (\s a -> s { _gusUser = a })
{-# INLINE gusUser #-}

instance FromXML GetUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetUser where
    type Sv GetUser = IAM
    type Rs GetUser = GetUserResponse

    request = post "GetUser"
    response _ = xmlResponse
