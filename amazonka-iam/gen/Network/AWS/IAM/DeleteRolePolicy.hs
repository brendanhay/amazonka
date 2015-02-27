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

-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified inline policy that is embedded in the specified role.
--
-- A role can also have managed policies attached to it. To detach a managed
-- policy from a role, use 'DetachRolePolicy'. For more information about
-- policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/
-- guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , deleteRolePolicy
    -- ** Request lenses
    , drp1PolicyName
    , drp1RoleName

    -- * Response
    , DeleteRolePolicyResponse
    -- ** Response constructor
    , deleteRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteRolePolicy = DeleteRolePolicy
    { _drp1PolicyName :: Text
    , _drp1RoleName   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drp1PolicyName' @::@ 'Text'
--
-- * 'drp1RoleName' @::@ 'Text'
--
deleteRolePolicy :: Text -- ^ 'drp1RoleName'
                 -> Text -- ^ 'drp1PolicyName'
                 -> DeleteRolePolicy
deleteRolePolicy p1 p2 = DeleteRolePolicy
    { _drp1RoleName   = p1
    , _drp1PolicyName = p2
    }

-- | The name identifying the policy document to delete.
drp1PolicyName :: Lens' DeleteRolePolicy Text
drp1PolicyName = lens _drp1PolicyName (\s a -> s { _drp1PolicyName = a })

-- | The name (friendly name, not ARN) identifying the role that the policy is
-- embedded in.
drp1RoleName :: Lens' DeleteRolePolicy Text
drp1RoleName = lens _drp1RoleName (\s a -> s { _drp1RoleName = a })

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteRolePolicyResponse' constructor.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse

instance ToPath DeleteRolePolicy where
    toPath = const "/"

instance ToQuery DeleteRolePolicy where
    toQuery DeleteRolePolicy{..} = mconcat
        [ "PolicyName" =? _drp1PolicyName
        , "RoleName"   =? _drp1RoleName
        ]

instance ToHeaders DeleteRolePolicy

instance AWSRequest DeleteRolePolicy where
    type Sv DeleteRolePolicy = IAM
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse

    request  = post "DeleteRolePolicy"
    response = nullResponse DeleteRolePolicyResponse
