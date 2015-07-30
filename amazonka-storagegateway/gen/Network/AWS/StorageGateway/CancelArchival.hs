{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS)
-- after the archiving process is initiated.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CancelArchival.html>
module Network.AWS.StorageGateway.CancelArchival
    (
    -- * Request
      CancelArchival
    -- ** Request constructor
    , cancelArchival
    -- ** Request lenses
    , caGatewayARN
    , caTapeARN

    -- * Response
    , CancelArchivalResponse
    -- ** Response constructor
    , cancelArchivalResponse
    -- ** Response lenses
    , carsTapeARN
    , carsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | CancelArchivalInput
--
-- /See:/ 'cancelArchival' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caGatewayARN'
--
-- * 'caTapeARN'
data CancelArchival = CancelArchival'
    { _caGatewayARN :: !Text
    , _caTapeARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelArchival' smart constructor.
cancelArchival :: Text -> Text -> CancelArchival
cancelArchival pGatewayARN_ pTapeARN_ =
    CancelArchival'
    { _caGatewayARN = pGatewayARN_
    , _caTapeARN = pTapeARN_
    }

-- | FIXME: Undocumented member.
caGatewayARN :: Lens' CancelArchival Text
caGatewayARN = lens _caGatewayARN (\ s a -> s{_caGatewayARN = a});

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel
-- archiving for.
caTapeARN :: Lens' CancelArchival Text
caTapeARN = lens _caTapeARN (\ s a -> s{_caTapeARN = a});

instance AWSRequest CancelArchival where
        type Sv CancelArchival = StorageGateway
        type Rs CancelArchival = CancelArchivalResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CancelArchivalResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance ToHeaders CancelArchival where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CancelArchival" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelArchival where
        toJSON CancelArchival'{..}
          = object
              ["GatewayARN" .= _caGatewayARN,
               "TapeARN" .= _caTapeARN]

instance ToPath CancelArchival where
        toPath = const mempty

instance ToQuery CancelArchival where
        toQuery = const mempty

-- | CancelArchivalOutput
--
-- /See:/ 'cancelArchivalResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carsTapeARN'
--
-- * 'carsStatus'
data CancelArchivalResponse = CancelArchivalResponse'
    { _carsTapeARN :: !(Maybe Text)
    , _carsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelArchivalResponse' smart constructor.
cancelArchivalResponse :: Int -> CancelArchivalResponse
cancelArchivalResponse pStatus_ =
    CancelArchivalResponse'
    { _carsTapeARN = Nothing
    , _carsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving
-- was canceled.
carsTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
carsTapeARN = lens _carsTapeARN (\ s a -> s{_carsTapeARN = a});

-- | FIXME: Undocumented member.
carsStatus :: Lens' CancelArchivalResponse Int
carsStatus = lens _carsStatus (\ s a -> s{_carsStatus = a});
