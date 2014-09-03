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
    , deletePolicy
    -- ** Request lenses
    , dptPolicyName
    , dptAutoScalingGroupName

    -- * Response
    , DeletePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeletePolicy' request.
deletePolicy :: Text -- ^ 'dptPolicyName'
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
    } deriving (Show, Generic)

-- | The name or PolicyARN of the policy you want to delete.
dptPolicyName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeletePolicy
    -> f DeletePolicy
dptPolicyName f x =
    (\y -> x { _dptPolicyName = y })
       <$> f (_dptPolicyName x)
{-# INLINE dptPolicyName #-}

-- | The name of the Auto Scaling group.
dptAutoScalingGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeletePolicy
    -> f DeletePolicy
dptAutoScalingGroupName f x =
    (\y -> x { _dptAutoScalingGroupName = y })
       <$> f (_dptAutoScalingGroupName x)
{-# INLINE dptAutoScalingGroupName #-}

instance ToQuery DeletePolicy where
    toQuery = genericQuery def

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeletePolicy where
    type Sv DeletePolicy = AutoScaling
    type Rs DeletePolicy = DeletePolicyResponse

    request = post "DeletePolicy"
    response _ = nullaryResponse DeletePolicyResponse
