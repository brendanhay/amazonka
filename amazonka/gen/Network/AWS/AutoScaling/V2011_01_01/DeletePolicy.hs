{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeletePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy created by PutScalingPolicy.
module Network.AWS.AutoScaling.V2011_01_01.DeletePolicy where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeletePolicy' request.
deletePolicy :: Text -- ^ '_dptPolicyName'
             -> DeletePolicy
deletePolicy p1 = DeletePolicy
    { _dptPolicyName = p1
    , _dptAutoScalingGroupName = Nothing
    }

data DeletePolicy = DeletePolicy
    { _dptPolicyName :: Text
      -- ^ The name or PolicyARN of the policy you want to delete.
    , _dptAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    } deriving (Generic)

instance ToQuery DeletePolicy where
    toQuery = genericToQuery def

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = AutoScaling
    type Rs DeletePolicy = DeletePolicyResponse

    request = post "DeletePolicy"
    response _ _ = return (Right DeletePolicyResponse)

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Show, Generic)
