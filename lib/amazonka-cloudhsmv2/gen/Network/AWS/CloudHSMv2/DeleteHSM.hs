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
-- Module      : Network.AWS.CloudHSMv2.DeleteHSM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM. To specify an HSM, you can use its identifier (ID), the IP address of the HSM's elastic network interface (ENI), or the ID of the HSM's ENI. You need to specify only one of these values. To find these values, use 'DescribeClusters' .
--
--
module Network.AWS.CloudHSMv2.DeleteHSM
    (
    -- * Creating a Request
      deleteHSM
    , DeleteHSM
    -- * Request Lenses
    , dhEniId
    , dhHSMId
    , dhEniIP
    , dhClusterId

    -- * Destructuring the Response
    , deleteHSMResponse
    , DeleteHSMResponse
    -- * Response Lenses
    , dhrsHSMId
    , dhrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteHSM' smart constructor.
data DeleteHSM = DeleteHSM'
  { _dhEniId     :: !(Maybe Text)
  , _dhHSMId     :: !(Maybe Text)
  , _dhEniIP     :: !(Maybe Text)
  , _dhClusterId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhEniId' - The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- * 'dhHSMId' - The identifier (ID) of the HSM that you are deleting.
--
-- * 'dhEniIP' - The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
--
-- * 'dhClusterId' - The identifier (ID) of the cluster that contains the HSM that you are deleting.
deleteHSM
    :: Text -- ^ 'dhClusterId'
    -> DeleteHSM
deleteHSM pClusterId_ =
  DeleteHSM'
    { _dhEniId = Nothing
    , _dhHSMId = Nothing
    , _dhEniIP = Nothing
    , _dhClusterId = pClusterId_
    }


-- | The identifier (ID) of the elastic network interface (ENI) of the HSM that you are deleting.
dhEniId :: Lens' DeleteHSM (Maybe Text)
dhEniId = lens _dhEniId (\ s a -> s{_dhEniId = a})

-- | The identifier (ID) of the HSM that you are deleting.
dhHSMId :: Lens' DeleteHSM (Maybe Text)
dhHSMId = lens _dhHSMId (\ s a -> s{_dhHSMId = a})

-- | The IP address of the elastic network interface (ENI) of the HSM that you are deleting.
dhEniIP :: Lens' DeleteHSM (Maybe Text)
dhEniIP = lens _dhEniIP (\ s a -> s{_dhEniIP = a})

-- | The identifier (ID) of the cluster that contains the HSM that you are deleting.
dhClusterId :: Lens' DeleteHSM Text
dhClusterId = lens _dhClusterId (\ s a -> s{_dhClusterId = a})

instance AWSRequest DeleteHSM where
        type Rs DeleteHSM = DeleteHSMResponse
        request = postJSON cloudHSMv2
        response
          = receiveJSON
              (\ s h x ->
                 DeleteHSMResponse' <$>
                   (x .?> "HsmId") <*> (pure (fromEnum s)))

instance Hashable DeleteHSM where

instance NFData DeleteHSM where

instance ToHeaders DeleteHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("BaldrApiService.DeleteHsm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHSM where
        toJSON DeleteHSM'{..}
          = object
              (catMaybes
                 [("EniId" .=) <$> _dhEniId,
                  ("HsmId" .=) <$> _dhHSMId, ("EniIp" .=) <$> _dhEniIP,
                  Just ("ClusterId" .= _dhClusterId)])

instance ToPath DeleteHSM where
        toPath = const "/"

instance ToQuery DeleteHSM where
        toQuery = const mempty

-- | /See:/ 'deleteHSMResponse' smart constructor.
data DeleteHSMResponse = DeleteHSMResponse'
  { _dhrsHSMId          :: !(Maybe Text)
  , _dhrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhrsHSMId' - The identifier (ID) of the HSM that was deleted.
--
-- * 'dhrsResponseStatus' - -- | The response status code.
deleteHSMResponse
    :: Int -- ^ 'dhrsResponseStatus'
    -> DeleteHSMResponse
deleteHSMResponse pResponseStatus_ =
  DeleteHSMResponse'
    {_dhrsHSMId = Nothing, _dhrsResponseStatus = pResponseStatus_}


-- | The identifier (ID) of the HSM that was deleted.
dhrsHSMId :: Lens' DeleteHSMResponse (Maybe Text)
dhrsHSMId = lens _dhrsHSMId (\ s a -> s{_dhrsHSMId = a})

-- | -- | The response status code.
dhrsResponseStatus :: Lens' DeleteHSMResponse Int
dhrsResponseStatus = lens _dhrsResponseStatus (\ s a -> s{_dhrsResponseStatus = a})

instance NFData DeleteHSMResponse where
