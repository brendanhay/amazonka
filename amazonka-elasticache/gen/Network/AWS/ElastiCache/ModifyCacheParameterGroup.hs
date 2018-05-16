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
-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
--
--
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
    (
    -- * Creating a Request
      modifyCacheParameterGroup
    , ModifyCacheParameterGroup
    -- * Request Lenses
    , mcpgCacheParameterGroupName
    , mcpgParameterNameValues

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

-- | Represents the input of a @ModifyCacheParameterGroup@ operation.
--
--
--
-- /See:/ 'modifyCacheParameterGroup' smart constructor.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'
  { _mcpgCacheParameterGroupName :: !Text
  , _mcpgParameterNameValues     :: ![ParameterNameValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcpgCacheParameterGroupName' - The name of the cache parameter group to modify.
--
-- * 'mcpgParameterNameValues' - An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
modifyCacheParameterGroup
    :: Text -- ^ 'mcpgCacheParameterGroupName'
    -> ModifyCacheParameterGroup
modifyCacheParameterGroup pCacheParameterGroupName_ =
  ModifyCacheParameterGroup'
    { _mcpgCacheParameterGroupName = pCacheParameterGroupName_
    , _mcpgParameterNameValues = mempty
    }


-- | The name of the cache parameter group to modify.
mcpgCacheParameterGroupName :: Lens' ModifyCacheParameterGroup Text
mcpgCacheParameterGroupName = lens _mcpgCacheParameterGroupName (\ s a -> s{_mcpgCacheParameterGroupName = a})

-- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
mcpgParameterNameValues :: Lens' ModifyCacheParameterGroup [ParameterNameValue]
mcpgParameterNameValues = lens _mcpgParameterNameValues (\ s a -> s{_mcpgParameterNameValues = a}) . _Coerce

instance AWSRequest ModifyCacheParameterGroup where
        type Rs ModifyCacheParameterGroup =
             CacheParameterGroupNameMessage
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ModifyCacheParameterGroupResult"
              (\ s h x -> parseXML x)

instance Hashable ModifyCacheParameterGroup where

instance NFData ModifyCacheParameterGroup where

instance ToHeaders ModifyCacheParameterGroup where
        toHeaders = const mempty

instance ToPath ModifyCacheParameterGroup where
        toPath = const "/"

instance ToQuery ModifyCacheParameterGroup where
        toQuery ModifyCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _mcpgCacheParameterGroupName,
               "ParameterNameValues" =:
                 toQueryList "ParameterNameValue"
                   _mcpgParameterNameValues]
