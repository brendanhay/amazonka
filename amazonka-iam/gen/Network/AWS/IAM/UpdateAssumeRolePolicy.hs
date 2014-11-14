{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the policy that grants an entity permission to assume a role. For
-- more information about roles, go to Working with Roles.
module Network.AWS.IAM.UpdateAssumeRolePolicy
    (
    -- * Request
      UpdateAssumeRolePolicy
    -- ** Request constructor
    , updateAssumeRolePolicy
    -- ** Request lenses
    , uarpPolicyDocument
    , uarpRoleName

    -- * Response
    , UpdateAssumeRolePolicyResponse
    -- ** Response constructor
    , updateAssumeRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { _uarpPolicyDocument :: Text
    , _uarpRoleName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAssumeRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarpPolicyDocument' @::@ 'Text'
--
-- * 'uarpRoleName' @::@ 'Text'
--
updateAssumeRolePolicy :: Text -- ^ 'uarpRoleName'
                       -> Text -- ^ 'uarpPolicyDocument'
                       -> UpdateAssumeRolePolicy
updateAssumeRolePolicy p1 p2 = UpdateAssumeRolePolicy
    { _uarpRoleName       = p1
    , _uarpPolicyDocument = p2
    }

-- | The policy that grants an entity permission to assume the role.
uarpPolicyDocument :: Lens' UpdateAssumeRolePolicy Text
uarpPolicyDocument =
    lens _uarpPolicyDocument (\s a -> s { _uarpPolicyDocument = a })

-- | The name of the role to update.
uarpRoleName :: Lens' UpdateAssumeRolePolicy Text
uarpRoleName = lens _uarpRoleName (\s a -> s { _uarpRoleName = a })

instance ToQuery UpdateAssumeRolePolicy

instance ToPath UpdateAssumeRolePolicy where
    toPath = const "/"

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAssumeRolePolicyResponse' constructor.
updateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse
updateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse

instance AWSRequest UpdateAssumeRolePolicy where
    type Sv UpdateAssumeRolePolicy = IAM
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse

    request  = post "UpdateAssumeRolePolicy"
    response = nullaryResponse UpdateAssumeRolePolicyResponse
