{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified role. The role must not have any policies attached.
-- For more information about roles, go to Working with Roles. Make sure you
-- do not have any Amazon EC2 instances running with the role you are about to
-- delete. Deleting a role or instance profile that is associated with a
-- running instance will break any applications running on the instance.
-- https://iam.amazonaws.com/ ?Action=DeleteRole &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS 913e3f37-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.DeleteRole
    (
    -- * Request
      DeleteRole
    -- ** Request constructor
    , deleteRole
    -- ** Request lenses
    , drRoleName

    -- * Response
    , DeleteRoleResponse
    -- ** Response constructor
    , deleteRoleResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteRole = DeleteRole
    { _drRoleName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRole' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
deleteRole :: Text -- ^ 'drRoleName'
           -> DeleteRole
deleteRole p1 = DeleteRole
    { _drRoleName = p1
    }

-- | Name of the role to delete.
drRoleName :: Lens' DeleteRole Text
drRoleName = lens _drRoleName (\s a -> s { _drRoleName = a })

instance ToQuery DeleteRole where
    toQuery = genericQuery def

data DeleteRoleResponse = DeleteRoleResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRoleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteRoleResponse :: DeleteRoleResponse
deleteRoleResponse = DeleteRoleResponse

instance AWSRequest DeleteRole where
    type Sv DeleteRole = IAM
    type Rs DeleteRole = DeleteRoleResponse

    request = post "DeleteRole"
    response _ = nullaryResponse DeleteRoleResponse
