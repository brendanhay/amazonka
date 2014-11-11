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

-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
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
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Request
      ResetCacheParameterGroupMessage
    -- ** Request constructor
    , resetCacheParameterGroupMessage
    -- ** Request lenses
    , rcpgmCacheParameterGroupName
    , rcpgmParameterNameValues
    , rcpgmResetAllParameters

    -- * Response
    , CacheParameterGroupNameMessage
    -- ** Response constructor
    , cacheParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data ResetCacheParameterGroupMessage = ResetCacheParameterGroupMessage
    { _rcpgmCacheParameterGroupName :: Text
    , _rcpgmParameterNameValues     :: [ParameterNameValue]
    , _rcpgmResetAllParameters      :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ResetCacheParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgmCacheParameterGroupName' @::@ 'Text'
--
-- * 'rcpgmParameterNameValues' @::@ ['ParameterNameValue']
--
-- * 'rcpgmResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetCacheParameterGroupMessage :: Text -- ^ 'rcpgmCacheParameterGroupName'
                                -> ResetCacheParameterGroupMessage
resetCacheParameterGroupMessage p1 = ResetCacheParameterGroupMessage
    { _rcpgmCacheParameterGroupName = p1
    , _rcpgmResetAllParameters      = Nothing
    , _rcpgmParameterNameValues     = mempty
    }

-- | The name of the cache parameter group to reset.
rcpgmCacheParameterGroupName :: Lens' ResetCacheParameterGroupMessage Text
rcpgmCacheParameterGroupName =
    lens _rcpgmCacheParameterGroupName
        (\s a -> s { _rcpgmCacheParameterGroupName = a })

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter
-- name.
rcpgmParameterNameValues :: Lens' ResetCacheParameterGroupMessage [ParameterNameValue]
rcpgmParameterNameValues =
    lens _rcpgmParameterNameValues
        (\s a -> s { _rcpgmParameterNameValues = a })

-- | If true, all parameters in the cache parameter group will be reset to
-- default values. If false, no such action occurs. Valid values: true |
-- false.
rcpgmResetAllParameters :: Lens' ResetCacheParameterGroupMessage (Maybe Bool)
rcpgmResetAllParameters =
    lens _rcpgmResetAllParameters (\s a -> s { _rcpgmResetAllParameters = a })
instance ToQuery ResetCacheParameterGroupMessage

instance ToPath ResetCacheParameterGroupMessage where
    toPath = const "/"

instance AWSRequest ResetCacheParameterGroupMessage where
    type Sv ResetCacheParameterGroupMessage = ElastiCache
    type Rs ResetCacheParameterGroupMessage = CacheParameterGroupNameMessage

    request  = post "ResetCacheParameterGroup"
    response = xmlResponse $ const decodeCursor
