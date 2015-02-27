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

-- Module      : Network.AWS.IAM.AttachGroupPolicy
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

-- | Attaches the specified managed policy to the specified group.
--
-- You use this API to attach a managed policy to a group. To embed an inline
-- policy in a group, use 'PutGroupPolicy'.
--
-- For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachGroupPolicy.html>
module Network.AWS.IAM.AttachGroupPolicy
    (
    -- * Request
      AttachGroupPolicy
    -- ** Request constructor
    , attachGroupPolicy
    -- ** Request lenses
    , agpGroupName
    , agpPolicyArn

    -- * Response
    , AttachGroupPolicyResponse
    -- ** Response constructor
    , attachGroupPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AttachGroupPolicy = AttachGroupPolicy
    { _agpGroupName :: Text
    , _agpPolicyArn :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agpGroupName' @::@ 'Text'
--
-- * 'agpPolicyArn' @::@ 'Text'
--
attachGroupPolicy :: Text -- ^ 'agpGroupName'
                  -> Text -- ^ 'agpPolicyArn'
                  -> AttachGroupPolicy
attachGroupPolicy p1 p2 = AttachGroupPolicy
    { _agpGroupName = p1
    , _agpPolicyArn = p2
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
agpGroupName :: Lens' AttachGroupPolicy Text
agpGroupName = lens _agpGroupName (\s a -> s { _agpGroupName = a })

agpPolicyArn :: Lens' AttachGroupPolicy Text
agpPolicyArn = lens _agpPolicyArn (\s a -> s { _agpPolicyArn = a })

data AttachGroupPolicyResponse = AttachGroupPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AttachGroupPolicyResponse' constructor.
attachGroupPolicyResponse :: AttachGroupPolicyResponse
attachGroupPolicyResponse = AttachGroupPolicyResponse

instance ToPath AttachGroupPolicy where
    toPath = const "/"

instance ToQuery AttachGroupPolicy where
    toQuery AttachGroupPolicy{..} = mconcat
        [ "GroupName" =? _agpGroupName
        , "PolicyArn" =? _agpPolicyArn
        ]

instance ToHeaders AttachGroupPolicy

instance AWSRequest AttachGroupPolicy where
    type Sv AttachGroupPolicy = IAM
    type Rs AttachGroupPolicy = AttachGroupPolicyResponse

    request  = post "AttachGroupPolicy"
    response = nullResponse AttachGroupPolicyResponse
