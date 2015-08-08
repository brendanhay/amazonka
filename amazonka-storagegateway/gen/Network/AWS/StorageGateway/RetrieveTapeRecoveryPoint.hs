{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
      RetrieveTapeRecoveryPoint
    , retrieveTapeRecoveryPoint
    -- * Request Lenses
    , rtrpTapeARN
    , rtrpGatewayARN

    -- * Destructuring the Response
    , RetrieveTapeRecoveryPointResponse
    , retrieveTapeRecoveryPointResponse
    -- * Response Lenses
    , rtrprsTapeARN
    , rtrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | RetrieveTapeRecoveryPointInput
--
-- /See:/ 'retrieveTapeRecoveryPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrpTapeARN'
--
-- * 'rtrpGatewayARN'
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
    { _rtrpTapeARN    :: !Text
    , _rtrpGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveTapeRecoveryPoint' smart constructor.
retrieveTapeRecoveryPoint :: Text -> Text -> RetrieveTapeRecoveryPoint
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
        type Sv RetrieveTapeRecoveryPoint = StorageGateway
        type Rs RetrieveTapeRecoveryPoint =
             RetrieveTapeRecoveryPointResponse
        request = postJSON
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
              ["TapeARN" .= _rtrpTapeARN,
               "GatewayARN" .= _rtrpGatewayARN]

instance ToPath RetrieveTapeRecoveryPoint where
        toPath = const "/"

instance ToQuery RetrieveTapeRecoveryPoint where
        toQuery = const mempty

-- | RetrieveTapeRecoveryPointOutput
--
-- /See:/ 'retrieveTapeRecoveryPointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrprsTapeARN'
--
-- * 'rtrprsStatus'
data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'
    { _rtrprsTapeARN :: !(Maybe Text)
    , _rtrprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveTapeRecoveryPointResponse' smart constructor.
retrieveTapeRecoveryPointResponse :: Int -> RetrieveTapeRecoveryPointResponse
retrieveTapeRecoveryPointResponse pStatus_ =
    RetrieveTapeRecoveryPointResponse'
    { _rtrprsTapeARN = Nothing
    , _rtrprsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
rtrprsTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrprsTapeARN = lens _rtrprsTapeARN (\ s a -> s{_rtrprsTapeARN = a});

-- | Undocumented member.
rtrprsStatus :: Lens' RetrieveTapeRecoveryPointResponse Int
rtrprsStatus = lens _rtrprsStatus (\ s a -> s{_rtrprsStatus = a});
