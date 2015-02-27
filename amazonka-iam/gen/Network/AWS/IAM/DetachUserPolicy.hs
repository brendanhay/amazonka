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

-- Module      : Network.AWS.IAM.DetachUserPolicy
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

-- | Removes the specified managed policy from the specified user.
--
-- A user can also have inline policies embedded with it. To delete an inline
-- policy, use the 'DeleteUserPolicy' API. For information about policies, refer
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachUserPolicy.html>
module Network.AWS.IAM.DetachUserPolicy
    (
    -- * Request
      DetachUserPolicy
    -- ** Request constructor
    , detachUserPolicy
    -- ** Request lenses
    , dup1PolicyArn
    , dup1UserName

    -- * Response
    , DetachUserPolicyResponse
    -- ** Response constructor
    , detachUserPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DetachUserPolicy = DetachUserPolicy
    { _dup1PolicyArn :: Text
    , _dup1UserName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachUserPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dup1PolicyArn' @::@ 'Text'
--
-- * 'dup1UserName' @::@ 'Text'
--
detachUserPolicy :: Text -- ^ 'dup1UserName'
                 -> Text -- ^ 'dup1PolicyArn'
                 -> DetachUserPolicy
detachUserPolicy p1 p2 = DetachUserPolicy
    { _dup1UserName  = p1
    , _dup1PolicyArn = p2
    }

dup1PolicyArn :: Lens' DetachUserPolicy Text
dup1PolicyArn = lens _dup1PolicyArn (\s a -> s { _dup1PolicyArn = a })

-- | The name (friendly name, not ARN) of the user to detach the policy from.
dup1UserName :: Lens' DetachUserPolicy Text
dup1UserName = lens _dup1UserName (\s a -> s { _dup1UserName = a })

data DetachUserPolicyResponse = DetachUserPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DetachUserPolicyResponse' constructor.
detachUserPolicyResponse :: DetachUserPolicyResponse
detachUserPolicyResponse = DetachUserPolicyResponse

instance ToPath DetachUserPolicy where
    toPath = const "/"

instance ToQuery DetachUserPolicy where
    toQuery DetachUserPolicy{..} = mconcat
        [ "PolicyArn" =? _dup1PolicyArn
        , "UserName"  =? _dup1UserName
        ]

instance ToHeaders DetachUserPolicy

instance AWSRequest DetachUserPolicy where
    type Sv DetachUserPolicy = IAM
    type Rs DetachUserPolicy = DetachUserPolicyResponse

    request  = post "DetachUserPolicy"
    response = nullResponse DetachUserPolicyResponse
