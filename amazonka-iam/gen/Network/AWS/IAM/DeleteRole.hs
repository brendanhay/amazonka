{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRole.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype DeleteRole = DeleteRole
    { _drRoleName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drRoleName' @::@ 'Text'
--
deleteRole :: Text -- ^ 'drRoleName'
           -> DeleteRole
deleteRole p1 = DeleteRole
    { _drRoleName = p1
    }

-- | The name of the role to delete.
drRoleName :: Lens' DeleteRole Text
drRoleName = lens _drRoleName (\s a -> s { _drRoleName = a })

data DeleteRoleResponse = DeleteRoleResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRoleResponse' constructor.
deleteRoleResponse :: DeleteRoleResponse
deleteRoleResponse = DeleteRoleResponse

instance ToPath DeleteRole where
    toPath = const "/"

instance ToQuery DeleteRole

instance ToHeaders DeleteRole

instance AWSRequest DeleteRole where
    type Sv DeleteRole = IAM
    type Rs DeleteRole = DeleteRoleResponse

    request  = post "DeleteRole"
    response = nullResponse DeleteRoleResponse
