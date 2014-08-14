{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a specified Elastic Load Balancing instance from its layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DetachElasticLoadBalancer where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DetachElasticLoadBalancer = DetachElasticLoadBalancer
    { _delbrElasticLoadBalancerName :: Text
      -- ^ The Elastic Load Balancing instance's name.
    , _delbrLayerId :: Text
      -- ^ The ID of the layer that the Elastic Load Balancing instance is
      -- attached to.
    } deriving (Show, Generic)

makeLenses ''DetachElasticLoadBalancer

instance ToPath DetachElasticLoadBalancer

instance ToQuery DetachElasticLoadBalancer

instance ToHeaders DetachElasticLoadBalancer

instance ToJSON DetachElasticLoadBalancer

data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse
    deriving (Eq, Show, Generic)

makeLenses ''DetachElasticLoadBalancerResponse

instance AWSRequest DetachElasticLoadBalancer where
    type Sv DetachElasticLoadBalancer = OpsWorks
    type Rs DetachElasticLoadBalancer = DetachElasticLoadBalancerResponse

    request = get
    response _ = nullaryResponse DetachElasticLoadBalancerResponse
