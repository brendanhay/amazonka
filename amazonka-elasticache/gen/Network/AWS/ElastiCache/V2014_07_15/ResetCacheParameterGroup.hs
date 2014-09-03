{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ResetCacheParameterGroup operation modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset the
-- entire cache parameter group, specify the ResetAllParameters and
-- CacheParameterGroupName parameters.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ResetCacheParameterGroup &ResetAllParameters=true
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup1
-- cb7cc855-b9d2-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.ResetCacheParameterGroup
    (
    -- * Request
      ResetCacheParameterGroup
    -- ** Request constructor
    , resetCacheParameterGroup
    -- ** Request lenses
    , rcpgmParameterNameValues
    , rcpgmCacheParameterGroupName
    , rcpgmResetAllParameters

    -- * Response
    , ResetCacheParameterGroupResponse
    -- ** Response lenses
    , cpgnnCacheParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ResetCacheParameterGroup' request.
resetCacheParameterGroup :: [ParameterNameValue] -- ^ 'rcpgmParameterNameValues'
                         -> Text -- ^ 'rcpgmCacheParameterGroupName'
                         -> ResetCacheParameterGroup
resetCacheParameterGroup p1 p2 = ResetCacheParameterGroup
    { _rcpgmParameterNameValues = p1
    , _rcpgmCacheParameterGroupName = p2
    , _rcpgmResetAllParameters = Nothing
    }

data ResetCacheParameterGroup = ResetCacheParameterGroup
    { _rcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names to be reset. If you are not resetting
      -- the entire cache parameter group, you must specify at least one
      -- parameter name.
    , _rcpgmCacheParameterGroupName :: Text
      -- ^ The name of the cache parameter group to reset.
    , _rcpgmResetAllParameters :: Maybe Bool
      -- ^ If true, all parameters in the cache parameter group will be
      -- reset to default values. If false, no such action occurs. Valid
      -- values: true | false.
    } deriving (Show, Generic)

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter name.
rcpgmParameterNameValues
    :: Functor f
    => ([ParameterNameValue]
    -> f ([ParameterNameValue]))
    -> ResetCacheParameterGroup
    -> f ResetCacheParameterGroup
rcpgmParameterNameValues f x =
    (\y -> x { _rcpgmParameterNameValues = y })
       <$> f (_rcpgmParameterNameValues x)
{-# INLINE rcpgmParameterNameValues #-}

-- | The name of the cache parameter group to reset.
rcpgmCacheParameterGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> ResetCacheParameterGroup
    -> f ResetCacheParameterGroup
rcpgmCacheParameterGroupName f x =
    (\y -> x { _rcpgmCacheParameterGroupName = y })
       <$> f (_rcpgmCacheParameterGroupName x)
{-# INLINE rcpgmCacheParameterGroupName #-}

-- | If true, all parameters in the cache parameter group will be reset to
-- default values. If false, no such action occurs. Valid values: true |
-- false.
rcpgmResetAllParameters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ResetCacheParameterGroup
    -> f ResetCacheParameterGroup
rcpgmResetAllParameters f x =
    (\y -> x { _rcpgmResetAllParameters = y })
       <$> f (_rcpgmResetAllParameters x)
{-# INLINE rcpgmResetAllParameters #-}

instance ToQuery ResetCacheParameterGroup where
    toQuery = genericQuery def

data ResetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { _cpgnnCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Show, Generic)

-- | The name of the cache parameter group.
cpgnnCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ResetCacheParameterGroupResponse
    -> f ResetCacheParameterGroupResponse
cpgnnCacheParameterGroupName f x =
    (\y -> x { _cpgnnCacheParameterGroupName = y })
       <$> f (_cpgnnCacheParameterGroupName x)
{-# INLINE cpgnnCacheParameterGroupName #-}

instance FromXML ResetCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ResetCacheParameterGroup where
    type Sv ResetCacheParameterGroup = ElastiCache
    type Rs ResetCacheParameterGroup = ResetCacheParameterGroupResponse

    request = post "ResetCacheParameterGroup"
    response _ = xmlResponse
