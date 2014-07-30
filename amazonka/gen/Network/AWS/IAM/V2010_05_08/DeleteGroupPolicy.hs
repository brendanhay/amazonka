{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy that is associated with the specified group.
-- https://iam.amazonaws.com/ ?Action=DeleteGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteGroupPolicy = DeleteGroupPolicy
    { _dgprGroupName :: Text
      -- ^ Name of the group the policy is associated with.
    , _dgprPolicyName :: Text
      -- ^ Name of the policy document to delete.
    } deriving (Generic)

instance ToQuery DeleteGroupPolicy where
    toQuery = genericToQuery def

instance AWSRequest DeleteGroupPolicy where
    type Sv DeleteGroupPolicy = IAM
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse

    request = post "DeleteGroupPolicy"
    response _ _ = return (Right DeleteGroupPolicyResponse)

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Show, Generic)
