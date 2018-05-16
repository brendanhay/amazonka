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
-- Module      : Network.AWS.DMS.RebootReplicationInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a replication instance. Rebooting results in a momentary outage, until the replication instance becomes available again.
--
--
module Network.AWS.DMS.RebootReplicationInstance
    (
    -- * Creating a Request
      rebootReplicationInstance
    , RebootReplicationInstance
    -- * Request Lenses
    , rriForceFailover
    , rriReplicationInstanceARN

    -- * Destructuring the Response
    , rebootReplicationInstanceResponse
    , RebootReplicationInstanceResponse
    -- * Response Lenses
    , rrirsReplicationInstance
    , rrirsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebootReplicationInstance' smart constructor.
data RebootReplicationInstance = RebootReplicationInstance'
  { _rriForceFailover          :: !(Maybe Bool)
  , _rriReplicationInstanceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rriForceFailover' - If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
--
-- * 'rriReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
rebootReplicationInstance
    :: Text -- ^ 'rriReplicationInstanceARN'
    -> RebootReplicationInstance
rebootReplicationInstance pReplicationInstanceARN_ =
  RebootReplicationInstance'
    { _rriForceFailover = Nothing
    , _rriReplicationInstanceARN = pReplicationInstanceARN_
    }


-- | If this parameter is @true@ , the reboot is conducted through a Multi-AZ failover. (If the instance isn't configured for Multi-AZ, then you can't specify @true@ .)
rriForceFailover :: Lens' RebootReplicationInstance (Maybe Bool)
rriForceFailover = lens _rriForceFailover (\ s a -> s{_rriForceFailover = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
rriReplicationInstanceARN :: Lens' RebootReplicationInstance Text
rriReplicationInstanceARN = lens _rriReplicationInstanceARN (\ s a -> s{_rriReplicationInstanceARN = a})

instance AWSRequest RebootReplicationInstance where
        type Rs RebootReplicationInstance =
             RebootReplicationInstanceResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 RebootReplicationInstanceResponse' <$>
                   (x .?> "ReplicationInstance") <*>
                     (pure (fromEnum s)))

instance Hashable RebootReplicationInstance where

instance NFData RebootReplicationInstance where

instance ToHeaders RebootReplicationInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.RebootReplicationInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RebootReplicationInstance where
        toJSON RebootReplicationInstance'{..}
          = object
              (catMaybes
                 [("ForceFailover" .=) <$> _rriForceFailover,
                  Just
                    ("ReplicationInstanceArn" .=
                       _rriReplicationInstanceARN)])

instance ToPath RebootReplicationInstance where
        toPath = const "/"

instance ToQuery RebootReplicationInstance where
        toQuery = const mempty

-- | /See:/ 'rebootReplicationInstanceResponse' smart constructor.
data RebootReplicationInstanceResponse = RebootReplicationInstanceResponse'
  { _rrirsReplicationInstance :: !(Maybe ReplicationInstance)
  , _rrirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrirsReplicationInstance' - The replication instance that is being rebooted.
--
-- * 'rrirsResponseStatus' - -- | The response status code.
rebootReplicationInstanceResponse
    :: Int -- ^ 'rrirsResponseStatus'
    -> RebootReplicationInstanceResponse
rebootReplicationInstanceResponse pResponseStatus_ =
  RebootReplicationInstanceResponse'
    { _rrirsReplicationInstance = Nothing
    , _rrirsResponseStatus = pResponseStatus_
    }


-- | The replication instance that is being rebooted.
rrirsReplicationInstance :: Lens' RebootReplicationInstanceResponse (Maybe ReplicationInstance)
rrirsReplicationInstance = lens _rrirsReplicationInstance (\ s a -> s{_rrirsReplicationInstance = a})

-- | -- | The response status code.
rrirsResponseStatus :: Lens' RebootReplicationInstanceResponse Int
rrirsResponseStatus = lens _rrirsResponseStatus (\ s a -> s{_rrirsResponseStatus = a})

instance NFData RebootReplicationInstanceResponse
         where
