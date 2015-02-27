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

-- Module      : Network.AWS.IAM.DeleteGroupPolicy
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

-- | Deletes the specified inline policy that is embedded in the specified group.
--
-- A group can also have managed policies attached to it. To detach a managed
-- policy from a group, use 'DetachGroupPolicy'. For more information about
-- policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/
-- guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
module Network.AWS.IAM.DeleteGroupPolicy
    (
    -- * Request
      DeleteGroupPolicy
    -- ** Request constructor
    , deleteGroupPolicy
    -- ** Request lenses
    , dgp1GroupName
    , dgp1PolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    -- ** Response constructor
    , deleteGroupPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteGroupPolicy = DeleteGroupPolicy
    { _dgp1GroupName  :: Text
    , _dgp1PolicyName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgp1GroupName' @::@ 'Text'
--
-- * 'dgp1PolicyName' @::@ 'Text'
--
deleteGroupPolicy :: Text -- ^ 'dgp1GroupName'
                  -> Text -- ^ 'dgp1PolicyName'
                  -> DeleteGroupPolicy
deleteGroupPolicy p1 p2 = DeleteGroupPolicy
    { _dgp1GroupName  = p1
    , _dgp1PolicyName = p2
    }

-- | The name (friendly name, not ARN) identifying the group that the policy is
-- embedded in.
dgp1GroupName :: Lens' DeleteGroupPolicy Text
dgp1GroupName = lens _dgp1GroupName (\s a -> s { _dgp1GroupName = a })

-- | The name identifying the policy document to delete.
dgp1PolicyName :: Lens' DeleteGroupPolicy Text
dgp1PolicyName = lens _dgp1PolicyName (\s a -> s { _dgp1PolicyName = a })

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteGroupPolicyResponse' constructor.
deleteGroupPolicyResponse :: DeleteGroupPolicyResponse
deleteGroupPolicyResponse = DeleteGroupPolicyResponse

instance ToPath DeleteGroupPolicy where
    toPath = const "/"

instance ToQuery DeleteGroupPolicy where
    toQuery DeleteGroupPolicy{..} = mconcat
        [ "GroupName"  =? _dgp1GroupName
        , "PolicyName" =? _dgp1PolicyName
        ]

instance ToHeaders DeleteGroupPolicy

instance AWSRequest DeleteGroupPolicy where
    type Sv DeleteGroupPolicy = IAM
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse

    request  = post "DeleteGroupPolicy"
    response = nullResponse DeleteGroupPolicyResponse
