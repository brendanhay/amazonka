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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the @ResetAllParameters@ and @CacheParameterGroupName@ parameters.
--
--
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Creating a Request
      resetCacheParameterGroup
    , ResetCacheParameterGroup
    -- * Request Lenses
    , rcpgResetAllParameters
    , rcpgParameterNameValues
    , rcpgCacheParameterGroupName

    -- * Destructuring the Response
    , cacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    -- * Response Lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
--
--
-- /See:/ 'resetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { _rcpgResetAllParameters      :: !(Maybe Bool)
  , _rcpgParameterNameValues     :: !(Maybe [ParameterNameValue])
  , _rcpgCacheParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetCacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcpgResetAllParameters' - If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values. Valid values: @true@ | @false@
--
-- * 'rcpgParameterNameValues' - An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
--
-- * 'rcpgCacheParameterGroupName' - The name of the cache parameter group to reset.
resetCacheParameterGroup
    :: Text -- ^ 'rcpgCacheParameterGroupName'
    -> ResetCacheParameterGroup
resetCacheParameterGroup pCacheParameterGroupName_ =
  ResetCacheParameterGroup'
    { _rcpgResetAllParameters = Nothing
    , _rcpgParameterNameValues = Nothing
    , _rcpgCacheParameterGroupName = pCacheParameterGroupName_
    }


-- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values. Valid values: @true@ | @false@
rcpgResetAllParameters :: Lens' ResetCacheParameterGroup (Maybe Bool)
rcpgResetAllParameters = lens _rcpgResetAllParameters (\ s a -> s{_rcpgResetAllParameters = a})

-- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
rcpgParameterNameValues :: Lens' ResetCacheParameterGroup [ParameterNameValue]
rcpgParameterNameValues = lens _rcpgParameterNameValues (\ s a -> s{_rcpgParameterNameValues = a}) . _Default . _Coerce

-- | The name of the cache parameter group to reset.
rcpgCacheParameterGroupName :: Lens' ResetCacheParameterGroup Text
rcpgCacheParameterGroupName = lens _rcpgCacheParameterGroupName (\ s a -> s{_rcpgCacheParameterGroupName = a})

instance AWSRequest ResetCacheParameterGroup where
        type Rs ResetCacheParameterGroup =
             CacheParameterGroupNameMessage
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ResetCacheParameterGroupResult"
              (\ s h x -> parseXML x)

instance Hashable ResetCacheParameterGroup where

instance NFData ResetCacheParameterGroup where

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
               "ParameterNameValues" =:
                 toQuery
                   (toQueryList "ParameterNameValue" <$>
                      _rcpgParameterNameValues),
               "CacheParameterGroupName" =:
                 _rcpgCacheParameterGroupName]
