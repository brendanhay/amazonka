{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Deletes the specified policy associated with the specified role.
-- https://iam.amazonaws.com/ ?Action=DeleteRolePolicy
-- &PolicyName=S3AccessPolicy &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS c749ee7f-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , deleteRolePolicy
    -- ** Request lenses
    , drpRoleName
    , drpPolicyName

    -- * Response
    , DeleteRolePolicyResponse
    -- ** Response constructor
    , deleteRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data DeleteRolePolicy = DeleteRolePolicy
    { _drpRoleName :: Text
    , _drpPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRolePolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
deleteRolePolicy :: Text -- ^ 'drpRoleName'
                 -> Text -- ^ 'drpPolicyName'
                 -> DeleteRolePolicy
deleteRolePolicy p1 p2 = DeleteRolePolicy
    { _drpRoleName = p1
    , _drpPolicyName = p2
    }

-- | Name of the role the associated with the policy.
drpRoleName :: Lens' DeleteRolePolicy Text
drpRoleName = lens _drpRoleName (\s a -> s { _drpRoleName = a })

-- | Name of the policy document to delete.
drpPolicyName :: Lens' DeleteRolePolicy Text
drpPolicyName = lens _drpPolicyName (\s a -> s { _drpPolicyName = a })

instance ToQuery DeleteRolePolicy where
    toQuery = genericQuery def

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRolePolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse

instance AWSRequest DeleteRolePolicy where
    type Sv DeleteRolePolicy = IAM
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse

    request = post "DeleteRolePolicy"
    response _ = nullaryResponse DeleteRolePolicyResponse
