{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RetrieveTapeArchive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an archived virtual tape from the virtual tape shelf (VTS) to
-- a gateway-VTL. Virtual tapes archived in the VTS are not associated with
-- any gateway. However after a tape is retrieved, it is associated with a
-- gateway, even though it is also listed in the VTS.
--
-- Once a tape is successfully retrieved to a gateway, it cannot be
-- retrieved again to another gateway. You must archive the tape again
-- before you can retrieve it to another gateway.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_RetrieveTapeArchive.html AWS API Reference> for RetrieveTapeArchive.
module Network.AWS.StorageGateway.RetrieveTapeArchive
    (
    -- * Creating a Request
      RetrieveTapeArchive
    , retrieveTapeArchive
    -- * Request Lenses
    , rtaTapeARN
    , rtaGatewayARN

    -- * Destructuring the Response
    , RetrieveTapeArchiveResponse
    , retrieveTapeArchiveResponse
    -- * Response Lenses
    , rtarsTapeARN
    , rtarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | RetrieveTapeArchiveInput
--
-- /See:/ 'retrieveTapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtaTapeARN'
--
-- * 'rtaGatewayARN'
data RetrieveTapeArchive = RetrieveTapeArchive'
    { _rtaTapeARN    :: !Text
    , _rtaGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveTapeArchive' smart constructor.
retrieveTapeArchive :: Text -> Text -> RetrieveTapeArchive
retrieveTapeArchive pTapeARN_ pGatewayARN_ =
    RetrieveTapeArchive'
    { _rtaTapeARN = pTapeARN_
    , _rtaGatewayARN = pGatewayARN_
    }

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
-- * 'rtarsTapeARN'
--
-- * 'rtarsStatus'
data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse'
    { _rtarsTapeARN :: !(Maybe Text)
    , _rtarsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveTapeArchiveResponse' smart constructor.
retrieveTapeArchiveResponse :: Int -> RetrieveTapeArchiveResponse
retrieveTapeArchiveResponse pStatus_ =
    RetrieveTapeArchiveResponse'
    { _rtarsTapeARN = Nothing
    , _rtarsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
rtarsTapeARN :: Lens' RetrieveTapeArchiveResponse (Maybe Text)
rtarsTapeARN = lens _rtarsTapeARN (\ s a -> s{_rtarsTapeARN = a});

-- | Undocumented member.
rtarsStatus :: Lens' RetrieveTapeArchiveResponse Int
rtarsStatus = lens _rtarsStatus (\ s a -> s{_rtarsStatus = a});
