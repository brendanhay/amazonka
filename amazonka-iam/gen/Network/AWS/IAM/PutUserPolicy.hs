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

-- Module      : Network.AWS.IAM.PutUserPolicy
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

-- | Adds (or updates) an inline policy document that is embedded in the specified
-- user.
--
-- A user can also have a managed policy attached to it. To attach a managed
-- policy to a user, use 'AttachUserPolicy'. To create a new managed policy, use 'CreatePolicy'. For information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you can
-- embed in a user, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than GET
-- when calling 'PutUserPolicy'. For general information about using the Query API
-- with IAM, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutUserPolicy.html>
module Network.AWS.IAM.PutUserPolicy
    (
    -- * Request
      PutUserPolicy
    -- ** Request constructor
    , putUserPolicy
    -- ** Request lenses
    , pupPolicyDocument
    , pupPolicyName
    , pupUserName

    -- * Response
    , PutUserPolicyResponse
    -- ** Response constructor
    , putUserPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data PutUserPolicy = PutUserPolicy
    { _pupPolicyDocument :: Text
    , _pupPolicyName     :: Text
    , _pupUserName       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PutUserPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pupPolicyDocument' @::@ 'Text'
--
-- * 'pupPolicyName' @::@ 'Text'
--
-- * 'pupUserName' @::@ 'Text'
--
putUserPolicy :: Text -- ^ 'pupUserName'
              -> Text -- ^ 'pupPolicyName'
              -> Text -- ^ 'pupPolicyDocument'
              -> PutUserPolicy
putUserPolicy p1 p2 p3 = PutUserPolicy
    { _pupUserName       = p1
    , _pupPolicyName     = p2
    , _pupPolicyDocument = p3
    }

-- | The policy document.
pupPolicyDocument :: Lens' PutUserPolicy Text
pupPolicyDocument =
    lens _pupPolicyDocument (\s a -> s { _pupPolicyDocument = a })

-- | The name of the policy document.
pupPolicyName :: Lens' PutUserPolicy Text
pupPolicyName = lens _pupPolicyName (\s a -> s { _pupPolicyName = a })

-- | The name of the user to associate the policy with.
pupUserName :: Lens' PutUserPolicy Text
pupUserName = lens _pupUserName (\s a -> s { _pupUserName = a })

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutUserPolicyResponse' constructor.
putUserPolicyResponse :: PutUserPolicyResponse
putUserPolicyResponse = PutUserPolicyResponse

instance ToPath PutUserPolicy where
    toPath = const "/"

instance ToQuery PutUserPolicy where
    toQuery PutUserPolicy{..} = mconcat
        [ "PolicyDocument" =? _pupPolicyDocument
        , "PolicyName"     =? _pupPolicyName
        , "UserName"       =? _pupUserName
        ]

instance ToHeaders PutUserPolicy

instance AWSRequest PutUserPolicy where
    type Sv PutUserPolicy = IAM
    type Rs PutUserPolicy = PutUserPolicyResponse

    request  = post "PutUserPolicy"
    response = nullResponse PutUserPolicyResponse
