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
-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.
--
--
module Network.AWS.StorageGateway.CancelArchival
    (
    -- * Creating a Request
      cancelArchival
    , CancelArchival
    -- * Request Lenses
    , caGatewayARN
    , caTapeARN

    -- * Destructuring the Response
    , cancelArchivalResponse
    , CancelArchivalResponse
    -- * Response Lenses
    , carsTapeARN
    , carsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | CancelArchivalInput
--
--
--
-- /See:/ 'cancelArchival' smart constructor.
data CancelArchival = CancelArchival'
  { _caGatewayARN :: !Text
  , _caTapeARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelArchival' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caGatewayARN' - Undocumented member.
--
-- * 'caTapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
cancelArchival
    :: Text -- ^ 'caGatewayARN'
    -> Text -- ^ 'caTapeARN'
    -> CancelArchival
cancelArchival pGatewayARN_ pTapeARN_ =
  CancelArchival' {_caGatewayARN = pGatewayARN_, _caTapeARN = pTapeARN_}


-- | Undocumented member.
caGatewayARN :: Lens' CancelArchival Text
caGatewayARN = lens _caGatewayARN (\ s a -> s{_caGatewayARN = a})

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
caTapeARN :: Lens' CancelArchival Text
caTapeARN = lens _caTapeARN (\ s a -> s{_caTapeARN = a})

instance AWSRequest CancelArchival where
        type Rs CancelArchival = CancelArchivalResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CancelArchivalResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

instance Hashable CancelArchival where

instance NFData CancelArchival where

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
              (catMaybes
                 [Just ("GatewayARN" .= _caGatewayARN),
                  Just ("TapeARN" .= _caTapeARN)])

instance ToPath CancelArchival where
        toPath = const "/"

instance ToQuery CancelArchival where
        toQuery = const mempty

-- | CancelArchivalOutput
--
--
--
-- /See:/ 'cancelArchivalResponse' smart constructor.
data CancelArchivalResponse = CancelArchivalResponse'
  { _carsTapeARN        :: !(Maybe Text)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelArchivalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsTapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
--
-- * 'carsResponseStatus' - -- | The response status code.
cancelArchivalResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CancelArchivalResponse
cancelArchivalResponse pResponseStatus_ =
  CancelArchivalResponse'
    {_carsTapeARN = Nothing, _carsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
carsTapeARN :: Lens' CancelArchivalResponse (Maybe Text)
carsTapeARN = lens _carsTapeARN (\ s a -> s{_carsTapeARN = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CancelArchivalResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CancelArchivalResponse where
