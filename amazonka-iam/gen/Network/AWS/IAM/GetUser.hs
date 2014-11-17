{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified user, including the user's
-- creation date, path, unique ID, and ARN. If you do not specify a user name,
-- IAM determines the user name implicitly based on the AWS access key ID used
-- to sign the request.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUser.html>
module Network.AWS.IAM.GetUser
    (
    -- * Request
      GetUser
    -- ** Request constructor
    , getUser
    -- ** Request lenses
    , guUserName

    -- * Response
    , GetUserResponse
    -- ** Response constructor
    , getUserResponse
    -- ** Response lenses
    , gurUser
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetUser = GetUser
    { _guUserName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guUserName' @::@ 'Maybe' 'Text'
--
getUser :: GetUser
getUser = GetUser
    { _guUserName = Nothing
    }

-- | The name of the user to get information about. This parameter is
-- optional. If it is not included, it defaults to the user making the
-- request.
guUserName :: Lens' GetUser (Maybe Text)
guUserName = lens _guUserName (\s a -> s { _guUserName = a })

newtype GetUserResponse = GetUserResponse
    { _gurUser :: User
    } deriving (Eq, Show, Generic)

-- | 'GetUserResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gurUser' @::@ 'User'
--
getUserResponse :: User -- ^ 'gurUser'
                -> GetUserResponse
getUserResponse p1 = GetUserResponse
    { _gurUser = p1
    }

-- | Information about the user.
gurUser :: Lens' GetUserResponse User
gurUser = lens _gurUser (\s a -> s { _gurUser = a })

instance ToPath GetUser where
    toPath = const "/"

instance ToQuery GetUser

instance ToHeaders GetUser

instance AWSRequest GetUser where
    type Sv GetUser = IAM
    type Rs GetUser = GetUserResponse

    request  = post "GetUser"
    response = xmlResponse

instance FromXML GetUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetUserResponse"
