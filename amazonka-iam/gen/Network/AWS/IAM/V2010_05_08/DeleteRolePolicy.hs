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
    , deleteRolePolicy
    -- ** Request lenses
    , drprPolicyName
    , drprRoleName

    -- * Response
    , DeleteRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteRolePolicy' request.
deleteRolePolicy :: Text -- ^ 'drprPolicyName'
                 -> Text -- ^ 'drprRoleName'
                 -> DeleteRolePolicy
deleteRolePolicy p1 p2 = DeleteRolePolicy
    { _drprPolicyName = p1
    , _drprRoleName = p2
    }
{-# INLINE deleteRolePolicy #-}

data DeleteRolePolicy = DeleteRolePolicy
    { _drprPolicyName :: Text
      -- ^ Name of the policy document to delete.
    , _drprRoleName :: Text
      -- ^ Name of the role the associated with the policy.
    } deriving (Show, Generic)

-- | Name of the policy document to delete.
drprPolicyName :: Lens' DeleteRolePolicy (Text)
drprPolicyName f x =
    f (_drprPolicyName x)
        <&> \y -> x { _drprPolicyName = y }
{-# INLINE drprPolicyName #-}

-- | Name of the role the associated with the policy.
drprRoleName :: Lens' DeleteRolePolicy (Text)
drprRoleName f x =
    f (_drprRoleName x)
        <&> \y -> x { _drprRoleName = y }
{-# INLINE drprRoleName #-}

instance ToQuery DeleteRolePolicy where
    toQuery = genericQuery def

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteRolePolicy where
    type Sv DeleteRolePolicy = IAM
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse

    request = post "DeleteRolePolicy"
    response _ = nullaryResponse DeleteRolePolicyResponse
