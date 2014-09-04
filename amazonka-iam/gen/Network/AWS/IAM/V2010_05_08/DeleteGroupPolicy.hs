{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
    (
    -- * Request
      DeleteGroupPolicy
    -- ** Request constructor
    , deleteGroupPolicy
    -- ** Request lenses
    , dgprGroupName
    , dgprPolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteGroupPolicy' request.
deleteGroupPolicy :: Text -- ^ 'dgprGroupName'
                  -> Text -- ^ 'dgprPolicyName'
                  -> DeleteGroupPolicy
deleteGroupPolicy p1 p2 = DeleteGroupPolicy
    { _dgprGroupName = p1
    , _dgprPolicyName = p2
    }
{-# INLINE deleteGroupPolicy #-}

data DeleteGroupPolicy = DeleteGroupPolicy
    { _dgprGroupName :: Text
      -- ^ Name of the group the policy is associated with.
    , _dgprPolicyName :: Text
      -- ^ Name of the policy document to delete.
    } deriving (Show, Generic)

-- | Name of the group the policy is associated with.
dgprGroupName :: Lens' DeleteGroupPolicy (Text)
dgprGroupName f x =
    f (_dgprGroupName x)
        <&> \y -> x { _dgprGroupName = y }
{-# INLINE dgprGroupName #-}

-- | Name of the policy document to delete.
dgprPolicyName :: Lens' DeleteGroupPolicy (Text)
dgprPolicyName f x =
    f (_dgprPolicyName x)
        <&> \y -> x { _dgprPolicyName = y }
{-# INLINE dgprPolicyName #-}

instance ToQuery DeleteGroupPolicy where
    toQuery = genericQuery def

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteGroupPolicy where
    type Sv DeleteGroupPolicy = IAM
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse

    request = post "DeleteGroupPolicy"
    response _ = nullaryResponse DeleteGroupPolicyResponse
