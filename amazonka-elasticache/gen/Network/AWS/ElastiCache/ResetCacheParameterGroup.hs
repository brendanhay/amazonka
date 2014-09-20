{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ResetCacheParameterGroup &ResetAllParameters=true
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup1
-- cb7cc855-b9d2-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Request
      ResetCacheParameterGroup
    -- ** Request constructor
    , resetCacheParameterGroup
    -- ** Request lenses
    , rcpgCacheParameterGroupName
    , rcpgResetAllParameters
    , rcpgParameterNameValues

    -- * Response
    , ResetCacheParameterGroupResponse
    -- ** Response constructor
    , resetCacheParameterGroupResponse
    -- ** Response lenses
    , rcpgrCacheParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a ResetCacheParameterGroup operation.
data ResetCacheParameterGroup = ResetCacheParameterGroup
    { _rcpgCacheParameterGroupName :: Text
    , _rcpgResetAllParameters :: Maybe Bool
    , _rcpgParameterNameValues :: [ParameterNameValue]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetCacheParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Text@
--
-- * @ResetAllParameters ::@ @Maybe Bool@
--
-- * @ParameterNameValues ::@ @[ParameterNameValue]@
--
resetCacheParameterGroup :: Text -- ^ 'rcpgCacheParameterGroupName'
                         -> [ParameterNameValue] -- ^ 'rcpgParameterNameValues'
                         -> ResetCacheParameterGroup
resetCacheParameterGroup p1 p3 = ResetCacheParameterGroup
    { _rcpgCacheParameterGroupName = p1
    , _rcpgResetAllParameters = Nothing
    , _rcpgParameterNameValues = p3
    }

-- | The name of the cache parameter group to reset.
rcpgCacheParameterGroupName :: Lens' ResetCacheParameterGroup Text
rcpgCacheParameterGroupName =
    lens _rcpgCacheParameterGroupName
         (\s a -> s { _rcpgCacheParameterGroupName = a })

-- | If true, all parameters in the cache parameter group will be reset to
-- default values. If false, no such action occurs. Valid values: true |
-- false.
rcpgResetAllParameters :: Lens' ResetCacheParameterGroup (Maybe Bool)
rcpgResetAllParameters =
    lens _rcpgResetAllParameters (\s a -> s { _rcpgResetAllParameters = a })

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter name.
rcpgParameterNameValues :: Lens' ResetCacheParameterGroup [ParameterNameValue]
rcpgParameterNameValues =
    lens _rcpgParameterNameValues
         (\s a -> s { _rcpgParameterNameValues = a })

instance ToQuery ResetCacheParameterGroup where
    toQuery = genericQuery def

-- | Represents the output of one of the following operations:
-- ModifyCacheParameterGroup ResetCacheParameterGroup.
newtype ResetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { _rcpgrCacheParameterGroupName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetCacheParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
resetCacheParameterGroupResponse :: ResetCacheParameterGroupResponse
resetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { _rcpgrCacheParameterGroupName = Nothing
    }

-- | The name of the cache parameter group.
rcpgrCacheParameterGroupName :: Lens' ResetCacheParameterGroupResponse (Maybe Text)
rcpgrCacheParameterGroupName =
    lens _rcpgrCacheParameterGroupName
         (\s a -> s { _rcpgrCacheParameterGroupName = a })

instance FromXML ResetCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ResetCacheParameterGroup where
    type Sv ResetCacheParameterGroup = ElastiCache
    type Rs ResetCacheParameterGroup = ResetCacheParameterGroupResponse

    request = post "ResetCacheParameterGroup"
    response _ = xmlResponse
