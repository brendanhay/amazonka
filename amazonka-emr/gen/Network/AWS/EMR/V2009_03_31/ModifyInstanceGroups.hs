{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ModifyInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | ModifyInstanceGroups modifies the number of nodes and configuration
-- settings of an instance group. The input parameters include the new target
-- instance count for the group and the instance group ID. The call will
-- either succeed or fail atomically. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target:
-- ElasticMapReduce.ModifyInstanceGroups Content-Length: 77 User-Agent:
-- aws-sdk-ruby/1.9.2 ruby/1.9.3 i386-mingw32 Host:
-- us-east-1.elasticmapreduce.amazonaws.com X-Amz-Date: 20130716T205843Z
-- X-Amz-Content-Sha256:
-- bb1af3d0c6c6a1a09f21ccd7f04a0e2e6c9ce5b5810b0f6777560fe4f81bda8c
-- Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20130716/us-east-1/elasticmapreduce/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-content-sha256;x-amz-date;x-amz-target,
-- Signature=17bbbb4448a1f47a14d5657445e9de5cadf16bed58b850585f80865882133b33
-- Accept: */* {"InstanceGroups": [{ "InstanceGroupId": "ig-1S8NWT31S2OVG",
-- "InstanceCount": 5 }]} HTTP/1.1 200 OK x-amzn-RequestId:
-- 80a74808-ee5a-11e2-90db-69a5154aeb8d Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Tue, 16 Jul 2013
-- 20:58:44 GMT.
module Network.AWS.EMR.V2009_03_31.ModifyInstanceGroups where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ModifyInstanceGroups' request.
modifyInstanceGroups :: ModifyInstanceGroups
modifyInstanceGroups = ModifyInstanceGroups
    { _migiInstanceGroups = mempty
    }

data ModifyInstanceGroups = ModifyInstanceGroups
    { _migiInstanceGroups :: [InstanceGroupModifyConfig]
      -- ^ Instance groups to change.
    } deriving (Show, Generic)

makeLenses ''ModifyInstanceGroups

instance ToPath ModifyInstanceGroups

instance ToQuery ModifyInstanceGroups

instance ToHeaders ModifyInstanceGroups

instance ToJSON ModifyInstanceGroups

data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse
    deriving (Eq, Show, Generic)

makeLenses ''ModifyInstanceGroupsResponse

instance AWSRequest ModifyInstanceGroups where
    type Sv ModifyInstanceGroups = EMR
    type Rs ModifyInstanceGroups = ModifyInstanceGroupsResponse

    request = get
    response _ = nullaryResponse ModifyInstanceGroupsResponse
