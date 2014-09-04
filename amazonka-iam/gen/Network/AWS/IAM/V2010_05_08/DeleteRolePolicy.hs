{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy associated with the specified role.
-- https://iam.amazonaws.com/ ?Action=DeleteRolePolicy
-- &PolicyName=S3AccessPolicy &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS c749ee7f-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , mkDeleteRolePolicyRequest
    -- ** Request lenses
    , drprRoleName
    , drprPolicyName

    -- * Response
    , DeleteRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRolePolicy' request.
mkDeleteRolePolicyRequest :: Text -- ^ 'drprRoleName'
                          -> Text -- ^ 'drprPolicyName'
                          -> DeleteRolePolicy
mkDeleteRolePolicyRequest p1 p2 = DeleteRolePolicy
    { _drprRoleName = p1
    , _drprPolicyName = p2
    }
{-# INLINE mkDeleteRolePolicyRequest #-}

data DeleteRolePolicy = DeleteRolePolicy
    { _drprRoleName :: Text
      -- ^ Name of the role the associated with the policy.
    , _drprPolicyName :: Text
      -- ^ Name of the policy document to delete.
    } deriving (Show, Generic)

-- | Name of the role the associated with the policy.
drprRoleName :: Lens' DeleteRolePolicy (Text)
drprRoleName = lens _drprRoleName (\s a -> s { _drprRoleName = a })
{-# INLINE drprRoleName #-}

-- | Name of the policy document to delete.
drprPolicyName :: Lens' DeleteRolePolicy (Text)
drprPolicyName = lens _drprPolicyName (\s a -> s { _drprPolicyName = a })
{-# INLINE drprPolicyName #-}

instance ToQuery DeleteRolePolicy where
    toQuery = genericQuery def

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteRolePolicy where
    type Sv DeleteRolePolicy = IAM
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse

    request = post "DeleteRolePolicy"
    response _ = nullaryResponse DeleteRolePolicyResponse
