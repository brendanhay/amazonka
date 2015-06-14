{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the recovery point for the specified virtual tape.
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
    , rtrpTapeARN
    , rtrpGatewayARN

    -- * Response
    , RetrieveTapeRecoveryPointResponse
    -- ** Response constructor
    , retrieveTapeRecoveryPointResponse
    -- ** Response lenses
    , rtrprTapeARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'retrieveTapeRecoveryPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrpTapeARN'
--
-- * 'rtrpGatewayARN'
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'{_rtrpTapeARN :: Text, _rtrpGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'RetrieveTapeRecoveryPoint' smart constructor.
retrieveTapeRecoveryPoint :: Text -> Text -> RetrieveTapeRecoveryPoint
retrieveTapeRecoveryPoint pTapeARN pGatewayARN = RetrieveTapeRecoveryPoint'{_rtrpTapeARN = pTapeARN, _rtrpGatewayARN = pGatewayARN};

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to
-- retrieve the recovery point.
rtrpTapeARN :: Lens' RetrieveTapeRecoveryPoint Text
rtrpTapeARN = lens _rtrpTapeARN (\ s a -> s{_rtrpTapeARN = a});

-- | FIXME: Undocumented member.
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
                   x .?> "TapeARN")

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

-- | /See:/ 'retrieveTapeRecoveryPointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrprTapeARN'
newtype RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'{_rtrprTapeARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RetrieveTapeRecoveryPointResponse' smart constructor.
retrieveTapeRecoveryPointResponse :: RetrieveTapeRecoveryPointResponse
retrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'{_rtrprTapeARN = Nothing};

-- | The Amazon Resource Name (ARN) of the virtual tape for which the
-- recovery point was retrieved.
rtrprTapeARN :: Lens' RetrieveTapeRecoveryPointResponse (Maybe Text)
rtrprTapeARN = lens _rtrprTapeARN (\ s a -> s{_rtrprTapeARN = a});
