{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheParameterGroup operation modifies the parameters of a cache
-- parameter group. You can modify up to 20 parameters in a single request by
-- submitting a list parameter name and value pairs.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyCacheParameterGroup
-- ?ParameterNameValues.member.1.ParameterName=chunk_size_growth_factor
-- &ParameterNameValues.member.1.ParameterValue=1.02
-- &CacheParameterGroupName=mycacheparametergroup &Version=2014-03-24
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T03%3A24%3A50.203Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycacheparametergroup
-- fcedeef2-b7ff-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { _mcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names and values for the parameter update.
      -- You must supply at least one parameter name and value; subsequent
      -- arguments are optional. A maximum of 20 parameters may be
      -- modified per request.
    , _mcpgmCacheParameterGroupName :: Text
      -- ^ The name of the cache parameter group to modify.
    } deriving (Show, Generic)

makeLenses ''ModifyCacheParameterGroup

instance ToQuery ModifyCacheParameterGroup where
    toQuery = genericToQuery def

data ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _cpgnmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Show, Generic)

makeLenses ''ModifyCacheParameterGroupResponse

instance AWSRequest ModifyCacheParameterGroup where
    type Sv ModifyCacheParameterGroup = ElastiCache
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse

    request = post "ModifyCacheParameterGroup"
    response _ = cursorResponse $ \hs xml ->
        pure ModifyCacheParameterGroupResponse
            <*> xml %|? "String"
