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
-- Module      : Network.AWS.DMS.RefreshSchemas
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Populates the schema for the specified endpoint. This is an asynchronous operation and can take several minutes. You can check the status of this operation by calling the DescribeRefreshSchemasStatus operation.
--
--
module Network.AWS.DMS.RefreshSchemas
    (
    -- * Creating a Request
      refreshSchemas
    , RefreshSchemas
    -- * Request Lenses
    , rsEndpointARN
    , rsReplicationInstanceARN

    -- * Destructuring the Response
    , refreshSchemasResponse
    , RefreshSchemasResponse
    -- * Response Lenses
    , rsrsRefreshSchemasStatus
    , rsrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'refreshSchemas' smart constructor.
data RefreshSchemas = RefreshSchemas'
  { _rsEndpointARN            :: !Text
  , _rsReplicationInstanceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RefreshSchemas' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rsReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
refreshSchemas
    :: Text -- ^ 'rsEndpointARN'
    -> Text -- ^ 'rsReplicationInstanceARN'
    -> RefreshSchemas
refreshSchemas pEndpointARN_ pReplicationInstanceARN_ =
  RefreshSchemas'
    { _rsEndpointARN = pEndpointARN_
    , _rsReplicationInstanceARN = pReplicationInstanceARN_
    }


-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rsEndpointARN :: Lens' RefreshSchemas Text
rsEndpointARN = lens _rsEndpointARN (\ s a -> s{_rsEndpointARN = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
rsReplicationInstanceARN :: Lens' RefreshSchemas Text
rsReplicationInstanceARN = lens _rsReplicationInstanceARN (\ s a -> s{_rsReplicationInstanceARN = a})

instance AWSRequest RefreshSchemas where
        type Rs RefreshSchemas = RefreshSchemasResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 RefreshSchemasResponse' <$>
                   (x .?> "RefreshSchemasStatus") <*>
                     (pure (fromEnum s)))

instance Hashable RefreshSchemas where

instance NFData RefreshSchemas where

instance ToHeaders RefreshSchemas where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.RefreshSchemas" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RefreshSchemas where
        toJSON RefreshSchemas'{..}
          = object
              (catMaybes
                 [Just ("EndpointArn" .= _rsEndpointARN),
                  Just
                    ("ReplicationInstanceArn" .=
                       _rsReplicationInstanceARN)])

instance ToPath RefreshSchemas where
        toPath = const "/"

instance ToQuery RefreshSchemas where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'refreshSchemasResponse' smart constructor.
data RefreshSchemasResponse = RefreshSchemasResponse'
  { _rsrsRefreshSchemasStatus :: !(Maybe RefreshSchemasStatus)
  , _rsrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RefreshSchemasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsrsRefreshSchemasStatus' - The status of the refreshed schema.
--
-- * 'rsrsResponseStatus' - -- | The response status code.
refreshSchemasResponse
    :: Int -- ^ 'rsrsResponseStatus'
    -> RefreshSchemasResponse
refreshSchemasResponse pResponseStatus_ =
  RefreshSchemasResponse'
    { _rsrsRefreshSchemasStatus = Nothing
    , _rsrsResponseStatus = pResponseStatus_
    }


-- | The status of the refreshed schema.
rsrsRefreshSchemasStatus :: Lens' RefreshSchemasResponse (Maybe RefreshSchemasStatus)
rsrsRefreshSchemasStatus = lens _rsrsRefreshSchemasStatus (\ s a -> s{_rsrsRefreshSchemasStatus = a})

-- | -- | The response status code.
rsrsResponseStatus :: Lens' RefreshSchemasResponse Int
rsrsResponseStatus = lens _rsrsResponseStatus (\ s a -> s{_rsrsResponseStatus = a})

instance NFData RefreshSchemasResponse where
