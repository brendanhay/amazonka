{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy from the load balancer. The specified policy must not be
-- enabled for any listeners.
module Network.AWS.ELB.DeleteLoadBalancerPolicy
    (
    -- * Request
      DeleteLoadBalancerPolicy
    -- ** Request constructor
    , deleteLoadBalancerPolicy
    -- ** Request lenses
    , dlbp1LoadBalancerName
    , dlbp1PolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , deleteLoadBalancerPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { _dlbp1LoadBalancerName :: Text
    , _dlbp1PolicyName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLoadBalancerPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbp1LoadBalancerName' @::@ 'Text'
--
-- * 'dlbp1PolicyName' @::@ 'Text'
--
deleteLoadBalancerPolicy :: Text -- ^ 'dlbp1LoadBalancerName'
                         -> Text -- ^ 'dlbp1PolicyName'
                         -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy p1 p2 = DeleteLoadBalancerPolicy
    { _dlbp1LoadBalancerName = p1
    , _dlbp1PolicyName       = p2
    }

-- | The mnemonic name associated with the load balancer.
dlbp1LoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
dlbp1LoadBalancerName =
    lens _dlbp1LoadBalancerName (\s a -> s { _dlbp1LoadBalancerName = a })

-- | The mnemonic name for the policy being deleted.
dlbp1PolicyName :: Lens' DeleteLoadBalancerPolicy Text
dlbp1PolicyName = lens _dlbp1PolicyName (\s a -> s { _dlbp1PolicyName = a })

instance ToQuery DeleteLoadBalancerPolicy

instance ToPath DeleteLoadBalancerPolicy where
    toPath = const "/"

data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLoadBalancerPolicyResponse' constructor.
deleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse

instance FromXML DeleteLoadBalancerPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLoadBalancerPolicyResponse"

instance AWSRequest DeleteLoadBalancerPolicy where
    type Sv DeleteLoadBalancerPolicy = ELB
    type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse

    request  = post "DeleteLoadBalancerPolicy"
    response = nullaryResponse DeleteLoadBalancerPolicyResponse
