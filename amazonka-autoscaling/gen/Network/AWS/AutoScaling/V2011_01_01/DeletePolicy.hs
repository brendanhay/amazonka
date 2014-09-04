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
    , mkDeletePolicyType
    -- ** Request lenses
    , dptAutoScalingGroupName
    , dptPolicyName

    -- * Response
    , DeletePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePolicy' request.
mkDeletePolicyType :: Text -- ^ 'dptPolicyName'
                   -> DeletePolicy
mkDeletePolicyType p1 = DeletePolicy
    { _dptAutoScalingGroupName = Nothing
    , _dptPolicyName = p2
    }
{-# INLINE mkDeletePolicyType #-}

data DeletePolicy = DeletePolicy
    { _dptAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dptPolicyName :: Text
      -- ^ The name or PolicyARN of the policy you want to delete.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dptAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dptAutoScalingGroupName = lens _dptAutoScalingGroupName (\s a -> s { _dptAutoScalingGroupName = a })
{-# INLINE dptAutoScalingGroupName #-}

-- | The name or PolicyARN of the policy you want to delete.
dptPolicyName :: Lens' DeletePolicy (Text)
dptPolicyName = lens _dptPolicyName (\s a -> s { _dptPolicyName = a })
{-# INLINE dptPolicyName #-}

instance ToQuery DeletePolicy where
    toQuery = genericQuery def

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = AutoScaling
    type Rs DeletePolicy = DeletePolicyResponse

    request = post "DeletePolicy"
    response _ = nullaryResponse DeletePolicyResponse
