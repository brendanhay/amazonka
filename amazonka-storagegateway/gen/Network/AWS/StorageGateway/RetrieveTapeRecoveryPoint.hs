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
-- Module      : Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the recovery point for the specified virtual tape.
--
-- A recovery point is a point in time view of a virtual tape at which all
-- the data on the tape is consistent. If your gateway crashes, virtual
-- tapes that have recovery points can be recovered to a new gateway.
--
-- The virtual tape can be retrieved to only one gateway. The retrieved
-- tape is read-only. The virtual tape can be retrieved to only a
-- gateway-VTL. There is no charge for retrieving recovery points.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_RetrieveTapeRecoveryPoint.html AWS API Reference> for RetrieveTapeRecoveryPoint.
module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
    (
    -- * Creating a Request
      retrieveTapeRecoveryPoint
    , RetrieveTapeRecoveryPoint
    -- * Request Lenses
    , rtrpTapeARN
    , rtrpGatewayARN

    -- * Destructuring the Response
    , retrieveTapeRecoveryPointResponse
    , RetrieveTapeRecoveryPointResponse
    -- * Response Lenses
    , rtrprsTapeARN
    , rtrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | RetrieveTapeRecoveryPointInput
--
-- /See:/ 'retrieveTapeRecoveryPoint' smart constructor.
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
    { _rtrpTapeARN    :: !Text
    , _rtrpGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetrieveTapeRecoveryPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrpTapeARN'
--
-- * 'rtrpGatewayARN'
retrieveTapeRecoveryPoint
    :: Text -- ^ 'rtrpTapeARN'
    -> Text -- ^ 'rtrpGatewayARN'
    -> RetrieveTapeRecoveryPoint
retrieveTapeRecoveryPoint pTapeARN_ pGatewayARN_ =
    RetrieveTapeRecoveryPoint'
    { _rtrpTapeARN = pTapeARN_
    , _rtrpGatewayARN = pGatewayARN_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to
-- retrieve the recovery point.
rtrpTapeARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrpTapeARN = lens _rtrpTapeARN (\ s a -> s{_rtrpTapeARN = a});

-- | Undocumented member.
rtrpGatewayARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrpGatewayARN = lens _rtrpGatewayARN (\ s a -> s{_rtrpGatewayARN = a});

instance AWSRequest RetrieveTapeRecoveryPoint where
        type Rs RetrieveTapeRecoveryPoint =
             RetrieveTapeRecoveryPointResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 RetrieveTapeRecoveryPointResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance ToHeaders RetrieveTapeRecoveryPoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.RetrieveTapeRecoveryPoint"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetrieveTapeRecoveryPoint where
        toJSON RetrieveTapeRecoveryPoint'{..}
          = object
              (catMaybes
                 [Just ("TapeARN" .= _rtrpTapeARN),
                  Just ("GatewayARN" .= _rtrpGatewayARN)])

instance ToPath RetrieveTapeRecoveryPoint where
        toPath = const "/"

instance ToQuery RetrieveTapeRecoveryPoint where
        toQuery = const mempty

-- | RetrieveTapeRecoveryPointOutput
--
-- /See:/ 'retrieveTapeRecoveryPointResponse' smart constructor.
data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'
    { _rtrprsTapeARN :: !(Maybe Text)
    , _rtrprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetrieveTapeRecoveryPointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrprsTapeARN'
--
-- * 'rtrprsStatus'
retrieveTapeRecoveryPointResponse
    :: Int -- ^ 'rtrprsStatus'
    -> RetrieveTapeRecoveryPointResponse
retrieveTapeRecoveryPointResponse pStatus_ =
    RetrieveTapeRecoveryPointResponse'
    { _rtrprsTapeARN = Nothing
    , _rtrprsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
rtrprsTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrprsTapeARN = lens _rtrprsTapeARN (\ s a -> s{_rtrprsTapeARN = a});

-- | The response status code.
rtrprsStatus :: Lens' RetrieveTapeRecoveryPointResponse Int
rtrprsStatus = lens _rtrprsStatus (\ s a -> s{_rtrprsStatus = a});
