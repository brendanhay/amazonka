{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified user. You should
-- understand the implications of changing a user's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide.
--
-- <UpdateUser.html>
module Network.AWS.IAM.UpdateUser
    (
    -- * Request
      UpdateUser
    -- ** Request constructor
    , updateUser
    -- ** Request lenses
    , uuNewPath
    , uuNewUserName
    , uuUserName

    -- * Response
    , UpdateUserResponse
    -- ** Response constructor
    , updateUserResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateUser = UpdateUser
    { _uuNewPath     :: Maybe Text
    , _uuNewUserName :: Maybe Text
    , _uuUserName    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uuNewPath' @::@ 'Maybe' 'Text'
--
-- * 'uuNewUserName' @::@ 'Maybe' 'Text'
--
-- * 'uuUserName' @::@ 'Text'
--
updateUser :: Text -- ^ 'uuUserName'
           -> UpdateUser
updateUser p1 = UpdateUser
    { _uuUserName    = p1
    , _uuNewPath     = Nothing
    , _uuNewUserName = Nothing
    }

-- | New path for the user. Include this parameter only if you're changing the
-- user's path.
uuNewPath :: Lens' UpdateUser (Maybe Text)
uuNewPath = lens _uuNewPath (\s a -> s { _uuNewPath = a })

-- | New name for the user. Include this parameter only if you're changing the
-- user's name.
uuNewUserName :: Lens' UpdateUser (Maybe Text)
uuNewUserName = lens _uuNewUserName (\s a -> s { _uuNewUserName = a })

-- | Name of the user to update. If you're changing the name of the user, this
-- is the original user name.
uuUserName :: Lens' UpdateUser Text
uuUserName = lens _uuUserName (\s a -> s { _uuUserName = a })

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateUserResponse' constructor.
updateUserResponse :: UpdateUserResponse
updateUserResponse = UpdateUserResponse

instance AWSRequest UpdateUser where
    type Sv UpdateUser = IAM
    type Rs UpdateUser = UpdateUserResponse

    request  = post "UpdateUser"
    response = nullResponse UpdateUserResponse

instance ToPath UpdateUser where
    toPath = const "/"

instance ToHeaders UpdateUser

instance ToQuery UpdateUser
