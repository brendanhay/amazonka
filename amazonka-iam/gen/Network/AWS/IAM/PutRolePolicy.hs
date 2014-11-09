{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in the Using IAM
-- guide. For information about limits on the policies you can associate with
-- a role, see Limitations on IAM Entities in the Using IAM guide.
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Request
      PutRolePolicy
    -- ** Request constructor
    , putRolePolicy
    -- ** Request lenses
    , prpPolicyDocument
    , prpPolicyName
    , prpRoleName

    -- * Response
    , PutRolePolicyResponse
    -- ** Response constructor
    , putRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data PutRolePolicy = PutRolePolicy
    { _prpPolicyDocument :: Text
    , _prpPolicyName     :: Text
    , _prpRoleName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prpPolicyDocument' @::@ 'Text'
--
-- * 'prpPolicyName' @::@ 'Text'
--
-- * 'prpRoleName' @::@ 'Text'
--
putRolePolicy :: Text -- ^ 'prpRoleName'
              -> Text -- ^ 'prpPolicyName'
              -> Text -- ^ 'prpPolicyDocument'
              -> PutRolePolicy
putRolePolicy p1 p2 p3 = PutRolePolicy
    { _prpRoleName       = p1
    , _prpPolicyName     = p2
    , _prpPolicyDocument = p3
    }

-- | The policy document.
prpPolicyDocument :: Lens' PutRolePolicy Text
prpPolicyDocument =
    lens _prpPolicyDocument (\s a -> s { _prpPolicyDocument = a })

-- | The name of the policy document.
prpPolicyName :: Lens' PutRolePolicy Text
prpPolicyName = lens _prpPolicyName (\s a -> s { _prpPolicyName = a })

-- | The name of the role to associate the policy with.
prpRoleName :: Lens' PutRolePolicy Text
prpRoleName = lens _prpRoleName (\s a -> s { _prpRoleName = a })

instance ToPath PutRolePolicy where
    toPath = const "/"

instance ToQuery PutRolePolicy

data PutRolePolicyResponse = PutRolePolicyResponse

-- | 'PutRolePolicyResponse' constructor.
putRolePolicyResponse :: PutRolePolicyResponse
putRolePolicyResponse = PutRolePolicyResponse

instance AWSRequest PutRolePolicy where
    type Sv PutRolePolicy = IAM
    type Rs PutRolePolicy = PutRolePolicyResponse

    request  = post "PutRolePolicy"
    response = const (nullaryResponse PutRolePolicyResponse)
