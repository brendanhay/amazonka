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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_RetrieveTapeRecoveryPoint.html>
module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
    (
    -- * Request
      RetrieveTapeRecoveryPoint
    -- ** Request constructor
    , retrieveTapeRecoveryPoint
    -- ** Request lenses
    , rtrprqTapeARN
    , rtrprqGatewayARN

    -- * Response
    , RetrieveTapeRecoveryPointResponse
    -- ** Response constructor
    , retrieveTapeRecoveryPointResponse
    -- ** Response lenses
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
-- * 'rtrprqTapeARN'
--
-- * 'rtrprqGatewayARN'
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
    { _rtrprqTapeARN    :: !Text
    , _rtrprqGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveTapeRecoveryPoint' smart constructor.
retrieveTapeRecoveryPoint :: Text -> Text -> RetrieveTapeRecoveryPoint
retrieveTapeRecoveryPoint pTapeARN pGatewayARN =
    RetrieveTapeRecoveryPoint'
    { _rtrprqTapeARN = pTapeARN
    , _rtrprqGatewayARN = pGatewayARN
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to
-- retrieve the recovery point.
rtrprqTapeARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrprqTapeARN = lens _rtrprqTapeARN (\ s a -> s{_rtrprqTapeARN = a});

-- | FIXME: Undocumented member.
rtrprqGatewayARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrprqGatewayARN = lens _rtrprqGatewayARN (\ s a -> s{_rtrprqGatewayARN = a});

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
              ["TapeARN" .= _rtrprqTapeARN,
               "GatewayARN" .= _rtrprqGatewayARN]

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
retrieveTapeRecoveryPointResponse pStatus =
    RetrieveTapeRecoveryPointResponse'
    { _rtrprsTapeARN = Nothing
    , _rtrprsStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
rtrprsTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrprsTapeARN = lens _rtrprsTapeARN (\ s a -> s{_rtrprsTapeARN = a});

-- | FIXME: Undocumented member.
rtrprsStatus :: Lens' RetrieveTapeRecoveryPointResponse Int
rtrprsStatus = lens _rtrprsStatus (\ s a -> s{_rtrprsStatus = a});
