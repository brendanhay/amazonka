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

-- Module      : Network.AWS.IAM.DetachGroupPolicy
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

-- | Removes the specified managed policy from the specified group.
--
-- A group can also have inline policies embedded with it. To delete an inline
-- policy, use the 'DeleteGroupPolicy' API. For information about policies, refer
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachGroupPolicy.html>
module Network.AWS.IAM.DetachGroupPolicy
    (
    -- * Request
      DetachGroupPolicy
    -- ** Request constructor
    , detachGroupPolicy
    -- ** Request lenses
    , dgpGroupName
    , dgpPolicyArn

    -- * Response
    , DetachGroupPolicyResponse
    -- ** Response constructor
    , detachGroupPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DetachGroupPolicy = DetachGroupPolicy
    { _dgpGroupName :: Text
    , _dgpPolicyArn :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgpGroupName' @::@ 'Text'
--
-- * 'dgpPolicyArn' @::@ 'Text'
--
detachGroupPolicy :: Text -- ^ 'dgpGroupName'
                  -> Text -- ^ 'dgpPolicyArn'
                  -> DetachGroupPolicy
detachGroupPolicy p1 p2 = DetachGroupPolicy
    { _dgpGroupName = p1
    , _dgpPolicyArn = p2
    }

-- | The name (friendly name, not ARN) of the group to detach the policy from.
dgpGroupName :: Lens' DetachGroupPolicy Text
dgpGroupName = lens _dgpGroupName (\s a -> s { _dgpGroupName = a })

dgpPolicyArn :: Lens' DetachGroupPolicy Text
dgpPolicyArn = lens _dgpPolicyArn (\s a -> s { _dgpPolicyArn = a })

data DetachGroupPolicyResponse = DetachGroupPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DetachGroupPolicyResponse' constructor.
detachGroupPolicyResponse :: DetachGroupPolicyResponse
detachGroupPolicyResponse = DetachGroupPolicyResponse

instance ToPath DetachGroupPolicy where
    toPath = const "/"

instance ToQuery DetachGroupPolicy where
    toQuery DetachGroupPolicy{..} = mconcat
        [ "GroupName" =? _dgpGroupName
        , "PolicyArn" =? _dgpPolicyArn
        ]

instance ToHeaders DetachGroupPolicy

instance AWSRequest DetachGroupPolicy where
    type Sv DetachGroupPolicy = IAM
    type Rs DetachGroupPolicy = DetachGroupPolicyResponse

    request  = post "DetachGroupPolicy"
    response = nullResponse DetachGroupPolicyResponse
