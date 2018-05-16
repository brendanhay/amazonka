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
-- Module      : Network.AWS.Config.BatchGetResourceConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration for one or more requested resources. The operation also returns a list of resources that are not processed in the current request. If there are no unprocessed resources, the operation returns an empty unprocessedResourceKeys list.
--
--
module Network.AWS.Config.BatchGetResourceConfig
    (
    -- * Creating a Request
      batchGetResourceConfig
    , BatchGetResourceConfig
    -- * Request Lenses
    , bgrcResourceKeys

    -- * Destructuring the Response
    , batchGetResourceConfigResponse
    , BatchGetResourceConfigResponse
    -- * Response Lenses
    , bgrcrsBaseConfigurationItems
    , bgrcrsUnprocessedResourceKeys
    , bgrcrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetResourceConfig' smart constructor.
newtype BatchGetResourceConfig = BatchGetResourceConfig'
  { _bgrcResourceKeys :: List1 ResourceKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrcResourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
batchGetResourceConfig
    :: NonEmpty ResourceKey -- ^ 'bgrcResourceKeys'
    -> BatchGetResourceConfig
batchGetResourceConfig pResourceKeys_ =
  BatchGetResourceConfig' {_bgrcResourceKeys = _List1 # pResourceKeys_}


-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
bgrcResourceKeys :: Lens' BatchGetResourceConfig (NonEmpty ResourceKey)
bgrcResourceKeys = lens _bgrcResourceKeys (\ s a -> s{_bgrcResourceKeys = a}) . _List1

instance AWSRequest BatchGetResourceConfig where
        type Rs BatchGetResourceConfig =
             BatchGetResourceConfigResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetResourceConfigResponse' <$>
                   (x .?> "baseConfigurationItems" .!@ mempty) <*>
                     (x .?> "unprocessedResourceKeys")
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetResourceConfig where

instance NFData BatchGetResourceConfig where

instance ToHeaders BatchGetResourceConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.BatchGetResourceConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetResourceConfig where
        toJSON BatchGetResourceConfig'{..}
          = object
              (catMaybes
                 [Just ("resourceKeys" .= _bgrcResourceKeys)])

instance ToPath BatchGetResourceConfig where
        toPath = const "/"

instance ToQuery BatchGetResourceConfig where
        toQuery = const mempty

-- | /See:/ 'batchGetResourceConfigResponse' smart constructor.
data BatchGetResourceConfigResponse = BatchGetResourceConfigResponse'
  { _bgrcrsBaseConfigurationItems  :: !(Maybe [BaseConfigurationItem])
  , _bgrcrsUnprocessedResourceKeys :: !(Maybe (List1 ResourceKey))
  , _bgrcrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetResourceConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgrcrsBaseConfigurationItems' - A list that contains the current configuration of one or more resources.
--
-- * 'bgrcrsUnprocessedResourceKeys' - A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
--
-- * 'bgrcrsResponseStatus' - -- | The response status code.
batchGetResourceConfigResponse
    :: Int -- ^ 'bgrcrsResponseStatus'
    -> BatchGetResourceConfigResponse
batchGetResourceConfigResponse pResponseStatus_ =
  BatchGetResourceConfigResponse'
    { _bgrcrsBaseConfigurationItems = Nothing
    , _bgrcrsUnprocessedResourceKeys = Nothing
    , _bgrcrsResponseStatus = pResponseStatus_
    }


-- | A list that contains the current configuration of one or more resources.
bgrcrsBaseConfigurationItems :: Lens' BatchGetResourceConfigResponse [BaseConfigurationItem]
bgrcrsBaseConfigurationItems = lens _bgrcrsBaseConfigurationItems (\ s a -> s{_bgrcrsBaseConfigurationItems = a}) . _Default . _Coerce

-- | A list of resource keys that were not processed with the current response. The unprocessesResourceKeys value is in the same form as ResourceKeys, so the value can be directly provided to a subsequent BatchGetResourceConfig operation. If there are no unprocessed resource keys, the response contains an empty unprocessedResourceKeys list.
bgrcrsUnprocessedResourceKeys :: Lens' BatchGetResourceConfigResponse (Maybe (NonEmpty ResourceKey))
bgrcrsUnprocessedResourceKeys = lens _bgrcrsUnprocessedResourceKeys (\ s a -> s{_bgrcrsUnprocessedResourceKeys = a}) . mapping _List1

-- | -- | The response status code.
bgrcrsResponseStatus :: Lens' BatchGetResourceConfigResponse Int
bgrcrsResponseStatus = lens _bgrcrsResponseStatus (\ s a -> s{_bgrcrsResponseStatus = a})

instance NFData BatchGetResourceConfigResponse where
