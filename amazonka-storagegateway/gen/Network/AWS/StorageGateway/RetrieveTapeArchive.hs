{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.RetrieveTapeArchive
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

-- | Retrieves an archived virtual tape from the virtual tape shelf (VTS) to
-- a gateway-VTL. Virtual tapes archived in the VTS are not associated with
-- any gateway. However after a tape is retrieved, it is associated with a
-- gateway, even though it is also listed in the VTS.
--
-- Once a tape is successfully retrieved to a gateway, it cannot be
-- retrieved again to another gateway. You must archive the tape again
-- before you can retrieve it to another gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_RetrieveTapeArchive.html>
module Network.AWS.StorageGateway.RetrieveTapeArchive
    (
    -- * Request
      RetrieveTapeArchive
    -- ** Request constructor
    , retrieveTapeArchive
    -- ** Request lenses
    , rtaTapeARN
    , rtaGatewayARN

    -- * Response
    , RetrieveTapeArchiveResponse
    -- ** Response constructor
    , retrieveTapeArchiveResponse
    -- ** Response lenses
    , rtarTapeARN
    , rtarStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | RetrieveTapeArchiveInput
--
-- /See:/ 'retrieveTapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtaTapeARN'
--
-- * 'rtaGatewayARN'
data RetrieveTapeArchive = RetrieveTapeArchive'{_rtaTapeARN :: Text, _rtaGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'RetrieveTapeArchive' smart constructor.
retrieveTapeArchive :: Text -> Text -> RetrieveTapeArchive
retrieveTapeArchive pTapeARN pGatewayARN = RetrieveTapeArchive'{_rtaTapeARN = pTapeARN, _rtaGatewayARN = pGatewayARN};

-- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve
-- from the virtual tape shelf (VTS).
rtaTapeARN :: Lens' RetrieveTapeArchive Text
rtaTapeARN = lens _rtaTapeARN (\ s a -> s{_rtaTapeARN = a});

-- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the
-- virtual tape to. Use the ListGateways operation to return a list of
-- gateways for your account and region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway
-- must be a gateway-VTL.
rtaGatewayARN :: Lens' RetrieveTapeArchive Text
rtaGatewayARN = lens _rtaGatewayARN (\ s a -> s{_rtaGatewayARN = a});

instance AWSRequest RetrieveTapeArchive where
        type Sv RetrieveTapeArchive = StorageGateway
        type Rs RetrieveTapeArchive =
             RetrieveTapeArchiveResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RetrieveTapeArchiveResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance ToHeaders RetrieveTapeArchive where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.RetrieveTapeArchive" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetrieveTapeArchive where
        toJSON RetrieveTapeArchive'{..}
          = object
              ["TapeARN" .= _rtaTapeARN,
               "GatewayARN" .= _rtaGatewayARN]

instance ToPath RetrieveTapeArchive where
        toPath = const "/"

instance ToQuery RetrieveTapeArchive where
        toQuery = const mempty

-- | RetrieveTapeArchiveOutput
--
-- /See:/ 'retrieveTapeArchiveResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtarTapeARN'
--
-- * 'rtarStatusCode'
data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse'{_rtarTapeARN :: Maybe Text, _rtarStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RetrieveTapeArchiveResponse' smart constructor.
retrieveTapeArchiveResponse :: Int -> RetrieveTapeArchiveResponse
retrieveTapeArchiveResponse pStatusCode = RetrieveTapeArchiveResponse'{_rtarTapeARN = Nothing, _rtarStatusCode = pStatusCode};

-- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
rtarTapeARN :: Lens' RetrieveTapeArchiveResponse (Maybe Text)
rtarTapeARN = lens _rtarTapeARN (\ s a -> s{_rtarTapeARN = a});

-- | FIXME: Undocumented member.
rtarStatusCode :: Lens' RetrieveTapeArchiveResponse Int
rtarStatusCode = lens _rtarStatusCode (\ s a -> s{_rtarStatusCode = a});
