{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateUser
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
-- the Using IAM guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUser.html>
module Network.AWS.IAM.CreateUser
    (
    -- * Request
      CreateUser
    -- ** Request constructor
    , createUser
    -- ** Request lenses
    , cuPath
    , cuUserName

    -- * Response
    , CreateUserResponse
    -- ** Response constructor
    , createUserResponse
    -- ** Response lenses
    , curUser
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateUser = CreateUser
    { _cuPath     :: Maybe Text
    , _cuUserName :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuPath' @::@ 'Maybe' 'Text'
--
-- * 'cuUserName' @::@ 'Text'
--
createUser :: Text -- ^ 'cuUserName'
           -> CreateUser
createUser p1 = CreateUser
    { _cuUserName = p1
    , _cuPath     = Nothing
    }

-- | The path for the user name. For more information about paths, see IAM
-- Identifiers in the Using IAM guide. This parameter is optional. If it is
-- not included, it defaults to a slash (/).
cuPath :: Lens' CreateUser (Maybe Text)
cuPath = lens _cuPath (\s a -> s { _cuPath = a })

-- | The name of the user to create.
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\s a -> s { _cuUserName = a })

newtype CreateUserResponse = CreateUserResponse
    { _curUser :: Maybe User
    } deriving (Eq, Show)

-- | 'CreateUserResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'curUser' @::@ 'Maybe' 'User'
--
createUserResponse :: CreateUserResponse
createUserResponse = CreateUserResponse
    { _curUser = Nothing
    }

-- | Information about the user.
curUser :: Lens' CreateUserResponse (Maybe User)
curUser = lens _curUser (\s a -> s { _curUser = a })

instance ToPath CreateUser where
    toPath = const "/"

instance ToQuery CreateUser where
    toQuery CreateUser{..} = mconcat
        [ "Path"     =? _cuPath
        , "UserName" =? _cuUserName
        ]

instance ToHeaders CreateUser

instance AWSRequest CreateUser where
    type Sv CreateUser = IAM
    type Rs CreateUser = CreateUserResponse

    request  = post "CreateUser"
    response = xmlResponse

instance FromXML CreateUserResponse where
    parseXML = withElement "CreateUserResult" $ \x -> CreateUserResponse
        <$> x .@? "User"
