{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteUser
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
-- https://iam.amazonaws.com/ ?Action=DeleteUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteUser
    (
    -- * Request
      DeleteUser
    -- ** Request constructor
    , mkDeleteUserRequest
    -- ** Request lenses
    , durUserName

    -- * Response
    , DeleteUserResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUser' request.
mkDeleteUserRequest :: Text -- ^ 'durUserName'
                    -> DeleteUser
mkDeleteUserRequest p1 = DeleteUser
    { _durUserName = p1
    }
{-# INLINE mkDeleteUserRequest #-}

newtype DeleteUser = DeleteUser
    { _durUserName :: Text
      -- ^ Name of the user to delete.
    } deriving (Show, Generic)

-- | Name of the user to delete.
durUserName :: Lens' DeleteUser (Text)
durUserName = lens _durUserName (\s a -> s { _durUserName = a })
{-# INLINE durUserName #-}

instance ToQuery DeleteUser where
    toQuery = genericQuery def

data DeleteUserResponse = DeleteUserResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteUser where
    type Sv DeleteUser = IAM
    type Rs DeleteUser = DeleteUserResponse

    request = post "DeleteUser"
    response _ = nullaryResponse DeleteUserResponse
