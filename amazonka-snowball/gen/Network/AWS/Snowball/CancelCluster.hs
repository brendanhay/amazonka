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
-- Module      : Network.AWS.Snowball.CancelCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a cluster job. You can only cancel a cluster job while it's in the @AwaitingQuorum@ status. You'll have at least an hour after creating a cluster job to cancel it.
--
--
module Network.AWS.Snowball.CancelCluster
    (
    -- * Creating a Request
      cancelCluster
    , CancelCluster
    -- * Request Lenses
    , ccClusterId

    -- * Destructuring the Response
    , cancelClusterResponse
    , CancelClusterResponse
    -- * Response Lenses
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'cancelCluster' smart constructor.
newtype CancelCluster = CancelCluster'
  { _ccClusterId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccClusterId' - The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
cancelCluster
    :: Text -- ^ 'ccClusterId'
    -> CancelCluster
cancelCluster pClusterId_ = CancelCluster' {_ccClusterId = pClusterId_}


-- | The 39-character ID for the cluster that you want to cancel, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
ccClusterId :: Lens' CancelCluster Text
ccClusterId = lens _ccClusterId (\ s a -> s{_ccClusterId = a})

instance AWSRequest CancelCluster where
        type Rs CancelCluster = CancelClusterResponse
        request = postJSON snowball
        response
          = receiveEmpty
              (\ s h x ->
                 CancelClusterResponse' <$> (pure (fromEnum s)))

instance Hashable CancelCluster where

instance NFData CancelCluster where

instance ToHeaders CancelCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.CancelCluster" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelCluster where
        toJSON CancelCluster'{..}
          = object
              (catMaybes [Just ("ClusterId" .= _ccClusterId)])

instance ToPath CancelCluster where
        toPath = const "/"

instance ToQuery CancelCluster where
        toQuery = const mempty

-- | /See:/ 'cancelClusterResponse' smart constructor.
newtype CancelClusterResponse = CancelClusterResponse'
  { _ccrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsResponseStatus' - -- | The response status code.
cancelClusterResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CancelClusterResponse
cancelClusterResponse pResponseStatus_ =
  CancelClusterResponse' {_ccrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccrsResponseStatus :: Lens' CancelClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CancelClusterResponse where
