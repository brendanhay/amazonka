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

-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
module Network.AWS.IAM.DeleteUser
    (
    -- * Request
      DeleteUser
    -- ** Request constructor
    , deleteUser
    -- ** Request lenses
    , duUserName

    -- * Response
    , DeleteUserResponse
    -- ** Response constructor
    , deleteUserResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype DeleteUser = DeleteUser
    { _duUserName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'duUserName' @::@ 'Text'
--
deleteUser :: Text -- ^ 'duUserName'
           -> DeleteUser
deleteUser p1 = DeleteUser
    { _duUserName = p1
    }

-- | The name of the user to delete.
duUserName :: Lens' DeleteUser Text
duUserName = lens _duUserName (\s a -> s { _duUserName = a })

instance ToQuery DeleteUser

instance ToPath DeleteUser where
    toPath = const "/"

data DeleteUserResponse = DeleteUserResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteUserResponse' constructor.
deleteUserResponse :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse

instance FromXML DeleteUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteUserResponse"

instance AWSRequest DeleteUser where
    type Sv DeleteUser = IAM
    type Rs DeleteUser = DeleteUserResponse

    request  = post "DeleteUser"
    response = nullaryResponse DeleteUserResponse
