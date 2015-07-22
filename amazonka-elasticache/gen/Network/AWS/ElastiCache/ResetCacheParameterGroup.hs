{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /ResetCacheParameterGroup/ action modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset
-- the entire cache parameter group, specify the /ResetAllParameters/ and
-- /CacheParameterGroupName/ parameters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ResetCacheParameterGroup.html>
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Request
      ResetCacheParameterGroup
    -- ** Request constructor
    , resetCacheParameterGroup
    -- ** Request lenses
    , rcpgrqResetAllParameters
    , rcpgrqCacheParameterGroupName
    , rcpgrqParameterNameValues

    -- * Response
    , CacheParameterGroupNameMessage
    -- ** Response constructor
    , cacheParameterGroupNameMessage
    -- ** Response lenses
    , rcpgrsCacheParameterGroupName
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ResetCacheParameterGroup/ action.
--
-- /See:/ 'resetCacheParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgrqResetAllParameters'
--
-- * 'rcpgrqCacheParameterGroupName'
--
-- * 'rcpgrqParameterNameValues'
data ResetCacheParameterGroup = ResetCacheParameterGroup'
    { _rcpgrqResetAllParameters      :: !(Maybe Bool)
    , _rcpgrqCacheParameterGroupName :: !Text
    , _rcpgrqParameterNameValues     :: ![ParameterNameValue]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetCacheParameterGroup' smart constructor.
resetCacheParameterGroup :: Text -> ResetCacheParameterGroup
resetCacheParameterGroup pCacheParameterGroupName =
    ResetCacheParameterGroup'
    { _rcpgrqResetAllParameters = Nothing
    , _rcpgrqCacheParameterGroupName = pCacheParameterGroupName
    , _rcpgrqParameterNameValues = mempty
    }

-- | If /true/, all parameters in the cache parameter group will be reset to
-- default values. If /false/, no such action occurs.
--
-- Valid values: @true@ | @false@
rcpgrqResetAllParameters :: Lens' ResetCacheParameterGroup (Maybe Bool)
rcpgrqResetAllParameters = lens _rcpgrqResetAllParameters (\ s a -> s{_rcpgrqResetAllParameters = a});

-- | The name of the cache parameter group to reset.
rcpgrqCacheParameterGroupName :: Lens' ResetCacheParameterGroup Text
rcpgrqCacheParameterGroupName = lens _rcpgrqCacheParameterGroupName (\ s a -> s{_rcpgrqCacheParameterGroupName = a});

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter
-- name.
rcpgrqParameterNameValues :: Lens' ResetCacheParameterGroup [ParameterNameValue]
rcpgrqParameterNameValues = lens _rcpgrqParameterNameValues (\ s a -> s{_rcpgrqParameterNameValues = a});

instance AWSRequest ResetCacheParameterGroup where
        type Sv ResetCacheParameterGroup = ElastiCache
        type Rs ResetCacheParameterGroup =
             CacheParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper "ResetCacheParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ResetCacheParameterGroup where
        toHeaders = const mempty

instance ToPath ResetCacheParameterGroup where
        toPath = const "/"

instance ToQuery ResetCacheParameterGroup where
        toQuery ResetCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ResetCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ResetAllParameters" =: _rcpgrqResetAllParameters,
               "CacheParameterGroupName" =:
                 _rcpgrqCacheParameterGroupName,
               "ParameterNameValues" =:
                 toQueryList "ParameterNameValue"
                   _rcpgrqParameterNameValues]
