{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DeletePolicy
    (
    -- * Request
      DeletePolicy
    -- ** Request constructor
    , mkDeletePolicy
    -- ** Request lenses
    , dpAutoScalingGroupName
    , dpPolicyName

    -- * Response
    , DeletePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data DeletePolicy = DeletePolicy
    { _dpAutoScalingGroupName :: Maybe Text
    , _dpPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePolicy' request.
mkDeletePolicy :: Text -- ^ 'dpPolicyName'
               -> DeletePolicy
mkDeletePolicy p2 = DeletePolicy
    { _dpAutoScalingGroupName = Nothing
    , _dpPolicyName = p2
    }
{-# INLINE mkDeletePolicy #-}

-- | The name of the Auto Scaling group.
dpAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dpAutoScalingGroupName =
    lens _dpAutoScalingGroupName (\s a -> s { _dpAutoScalingGroupName = a })
{-# INLINE dpAutoScalingGroupName #-}

-- | The name or PolicyARN of the policy you want to delete.
dpPolicyName :: Lens' DeletePolicy Text
dpPolicyName = lens _dpPolicyName (\s a -> s { _dpPolicyName = a })
{-# INLINE dpPolicyName #-}

instance ToQuery DeletePolicy where
    toQuery = genericQuery def

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = AutoScaling
    type Rs DeletePolicy = DeletePolicyResponse

    request = post "DeletePolicy"
    response _ = nullaryResponse DeletePolicyResponse
