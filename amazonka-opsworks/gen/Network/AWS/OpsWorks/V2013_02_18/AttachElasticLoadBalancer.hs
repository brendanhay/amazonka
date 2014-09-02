{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Elastic Load Balancing load balancer to a specified layer. For
-- more information, see Elastic Load Balancing. You must create the Elastic
-- Load Balancing instance separately, by using the Elastic Load Balancing
-- console, API, or CLI. For more information, see Elastic Load Balancing
-- Developer Guide. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.AttachElasticLoadBalancer where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data AttachElasticLoadBalancer = AttachElasticLoadBalancer
    { _aelbrLayerId :: Text
      -- ^ The ID of the layer that the Elastic Load Balancing instance is
      -- to be attached to.
    , _aelbrElasticLoadBalancerName :: Text
      -- ^ The Elastic Load Balancing instance's name.
    } deriving (Show, Generic)

makeLenses ''AttachElasticLoadBalancer

instance ToPath AttachElasticLoadBalancer

instance ToQuery AttachElasticLoadBalancer

instance ToHeaders AttachElasticLoadBalancer

instance ToJSON AttachElasticLoadBalancer

data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse
    deriving (Eq, Show, Generic)

makeLenses ''AttachElasticLoadBalancerResponse

instance AWSRequest AttachElasticLoadBalancer where
    type Sv AttachElasticLoadBalancer = OpsWorks
    type Rs AttachElasticLoadBalancer = AttachElasticLoadBalancerResponse

    request = get
    response _ = nullaryResponse AttachElasticLoadBalancerResponse
