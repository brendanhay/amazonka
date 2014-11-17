{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , deleteRolePolicy
    -- ** Request lenses
    , drpPolicyName
    , drpRoleName

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
    { _drpPolicyName :: Text
    , _drpRoleName   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drpPolicyName' @::@ 'Text'
--
-- * 'drpRoleName' @::@ 'Text'
--
deleteRolePolicy :: Text -- ^ 'drpRoleName'
                 -> Text -- ^ 'drpPolicyName'
                 -> DeleteRolePolicy
deleteRolePolicy p1 p2 = DeleteRolePolicy
    { _drpRoleName   = p1
    , _drpPolicyName = p2
    }

-- | The name of the policy document to delete.
drpPolicyName :: Lens' DeleteRolePolicy Text
drpPolicyName = lens _drpPolicyName (\s a -> s { _drpPolicyName = a })

-- | The name of the role the associated with the policy.
drpRoleName :: Lens' DeleteRolePolicy Text
drpRoleName = lens _drpRoleName (\s a -> s { _drpRoleName = a })

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRolePolicyResponse' constructor.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse

instance AWSRequest DeleteRolePolicy where
    type Sv DeleteRolePolicy = IAM
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse

    request  = post "DeleteRolePolicy"
    response = nullResponse DeleteRolePolicyResponse

instance ToPath DeleteRolePolicy where
    toPath = const "/"

instance ToHeaders DeleteRolePolicy

instance ToQuery DeleteRolePolicy
