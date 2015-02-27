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

-- Module      : Network.AWS.IAM.DeletePolicy
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

-- | Deletes the specified managed policy.
--
-- Before you can delete a managed policy, you must detach the policy from all
-- users, groups, and roles that it is attached to, and you must delete all of
-- the policy's versions. The following steps describe the process for deleting
-- a managed policy:  Detach the policy from all users, groups, and roles that
-- the policy is attached to, using the 'DetachUserPolicy', 'DetachGroupPolicy', or 'DetachRolePolicy' APIs. To list all the users, groups, and roles that a policy is attached to,
-- use 'ListEntitiesForPolicy'.  Delete all versions of the policy using 'DeletePolicyVersion'. To list the policy's versions, use 'ListPolicyVersions'. You cannot use 'DeletePolicyVersion' to delete the version that is marked as the default version. You delete the
-- policy's default version in the next step of the process.  Delete the policy
-- (this automatically deletes the policy's default version) using this API.
--
-- For information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeletePolicy.html>
module Network.AWS.IAM.DeletePolicy
    (
    -- * Request
      DeletePolicy
    -- ** Request constructor
    , deletePolicy
    -- ** Request lenses
    , dpPolicyArn

    -- * Response
    , DeletePolicyResponse
    -- ** Response constructor
    , deletePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype DeletePolicy = DeletePolicy
    { _dpPolicyArn :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeletePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpPolicyArn' @::@ 'Text'
--
deletePolicy :: Text -- ^ 'dpPolicyArn'
             -> DeletePolicy
deletePolicy p1 = DeletePolicy
    { _dpPolicyArn = p1
    }

dpPolicyArn :: Lens' DeletePolicy Text
dpPolicyArn = lens _dpPolicyArn (\s a -> s { _dpPolicyArn = a })

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeletePolicyResponse' constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse

instance ToPath DeletePolicy where
    toPath = const "/"

instance ToQuery DeletePolicy where
    toQuery DeletePolicy{..} = mconcat
        [ "PolicyArn" =? _dpPolicyArn
        ]

instance ToHeaders DeletePolicy

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = IAM
    type Rs DeletePolicy = DeletePolicyResponse

    request  = post "DeletePolicy"
    response = nullResponse DeletePolicyResponse
