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
    , mkGetUser
    -- ** Request lenses
    , guUserName

    -- * Response
    , GetUserResponse
    -- ** Response lenses
    , gursUser
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
newtype GetUser = GetUser
    { _guUserName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetUser' request.
mkGetUser :: GetUser
mkGetUser = GetUser
    { _guUserName = Nothing
    }
{-# INLINE mkGetUser #-}

-- | Name of the user to get information about. This parameter is optional. If
-- it is not included, it defaults to the user making the request.
guUserName :: Lens' GetUser (Maybe Text)
guUserName = lens _guUserName (\s a -> s { _guUserName = a })
{-# INLINE guUserName #-}

instance ToQuery GetUser where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetUser action.
newtype GetUserResponse = GetUserResponse
    { _gursUser :: User
    } deriving (Show, Generic)

-- | Information about the user.
gursUser :: Lens' GetUserResponse User
gursUser = lens _gursUser (\s a -> s { _gursUser = a })
{-# INLINE gursUser #-}

instance FromXML GetUserResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetUser where
    type Sv GetUser = IAM
    type Rs GetUser = GetUserResponse

    request = post "GetUser"
    response _ = xmlResponse
