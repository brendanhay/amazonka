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
-- Module      : Network.AWS.SMS.DisassociateConnector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DisassociateConnector API will disassociate a connector from the Server Migration Service, rendering it unavailable to support replication jobs.
module Network.AWS.SMS.DisassociateConnector
    (
    -- * Creating a Request
      disassociateConnector
    , DisassociateConnector
    -- * Request Lenses
    , dcConnectorId

    -- * Destructuring the Response
    , disassociateConnectorResponse
    , DisassociateConnectorResponse
    -- * Response Lenses
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'disassociateConnector' smart constructor.
newtype DisassociateConnector = DisassociateConnector'
  { _dcConnectorId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateConnector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcConnectorId' - Undocumented member.
disassociateConnector
    :: Text -- ^ 'dcConnectorId'
    -> DisassociateConnector
disassociateConnector pConnectorId_ =
  DisassociateConnector' {_dcConnectorId = pConnectorId_}


-- | Undocumented member.
dcConnectorId :: Lens' DisassociateConnector Text
dcConnectorId = lens _dcConnectorId (\ s a -> s{_dcConnectorId = a})

instance AWSRequest DisassociateConnector where
        type Rs DisassociateConnector =
             DisassociateConnectorResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateConnectorResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateConnector where

instance NFData DisassociateConnector where

instance ToHeaders DisassociateConnector where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.DisassociateConnector"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateConnector where
        toJSON DisassociateConnector'{..}
          = object
              (catMaybes [Just ("connectorId" .= _dcConnectorId)])

instance ToPath DisassociateConnector where
        toPath = const "/"

instance ToQuery DisassociateConnector where
        toQuery = const mempty

-- | /See:/ 'disassociateConnectorResponse' smart constructor.
newtype DisassociateConnectorResponse = DisassociateConnectorResponse'
  { _dcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateConnectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
disassociateConnectorResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DisassociateConnectorResponse
disassociateConnectorResponse pResponseStatus_ =
  DisassociateConnectorResponse' {_dcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcrsResponseStatus :: Lens' DisassociateConnectorResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DisassociateConnectorResponse where
