{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /ResetCacheParameterGroup/ action modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset
-- the entire cache parameter group, specify the /ResetAllParameters/ and
-- /CacheParameterGroupName/ parameters.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ResetCacheParameterGroup.html AWS API Reference> for ResetCacheParameterGroup.
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Creating a Request
      resetCacheParameterGroup
    , ResetCacheParameterGroup
    -- * Request Lenses
    , rcpgResetAllParameters
    , rcpgCacheParameterGroupName
    , rcpgParameterNameValues

    -- * Destructuring the Response
    , cacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    -- * Response Lenses
    , cpgnmCacheParameterGroupName
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ResetCacheParameterGroup/ action.
--
-- /See:/ 'resetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
    { _rcpgResetAllParameters      :: !(Maybe Bool)
    , _rcpgCacheParameterGroupName :: !Text
    , _rcpgParameterNameValues     :: ![ParameterNameValue]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResetCacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcpgResetAllParameters'
--
-- * 'rcpgCacheParameterGroupName'
--
-- * 'rcpgParameterNameValues'
resetCacheParameterGroup
    :: Text -- ^ 'rcpgCacheParameterGroupName'
    -> ResetCacheParameterGroup
resetCacheParameterGroup pCacheParameterGroupName_ =
    ResetCacheParameterGroup'
    { _rcpgResetAllParameters = Nothing
    , _rcpgCacheParameterGroupName = pCacheParameterGroupName_
    , _rcpgParameterNameValues = mempty
    }

-- | If /true/, all parameters in the cache parameter group will be reset to
-- default values. If /false/, no such action occurs.
--
-- Valid values: 'true' | 'false'
rcpgResetAllParameters :: Lens' ResetCacheParameterGroup (Maybe Bool)
rcpgResetAllParameters = lens _rcpgResetAllParameters (\ s a -> s{_rcpgResetAllParameters = a});

-- | The name of the cache parameter group to reset.
rcpgCacheParameterGroupName :: Lens' ResetCacheParameterGroup Text
rcpgCacheParameterGroupName = lens _rcpgCacheParameterGroupName (\ s a -> s{_rcpgCacheParameterGroupName = a});

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter
-- name.
rcpgParameterNameValues :: Lens' ResetCacheParameterGroup [ParameterNameValue]
rcpgParameterNameValues = lens _rcpgParameterNameValues (\ s a -> s{_rcpgParameterNameValues = a}) . _Coerce;

instance AWSRequest ResetCacheParameterGroup where
        type Rs ResetCacheParameterGroup =
             CacheParameterGroupNameMessage
        request = postQuery elastiCache
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
               "ResetAllParameters" =: _rcpgResetAllParameters,
               "CacheParameterGroupName" =:
                 _rcpgCacheParameterGroupName,
               "ParameterNameValues" =:
                 toQueryList "ParameterNameValue"
                   _rcpgParameterNameValues]
