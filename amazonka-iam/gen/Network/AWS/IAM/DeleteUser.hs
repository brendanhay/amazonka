{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- https://iam.amazonaws.com/ ?Action=DeleteUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
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

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteUser = DeleteUser
    { _duUserName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUser' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
deleteUser :: Text -- ^ 'duUserName'
           -> DeleteUser
deleteUser p1 = DeleteUser
    { _duUserName = p1
    }

-- | Name of the user to delete.
duUserName :: Lens' DeleteUser Text
duUserName = lens _duUserName (\s a -> s { _duUserName = a })

instance ToQuery DeleteUser where
    toQuery = genericQuery def

data DeleteUserResponse = DeleteUserResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUserResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteUserResponse :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse

instance AWSRequest DeleteUser where
    type Sv DeleteUser = IAM
    type Rs DeleteUser = DeleteUserResponse

    request = post "DeleteUser"
    response _ = nullaryResponse DeleteUserResponse
