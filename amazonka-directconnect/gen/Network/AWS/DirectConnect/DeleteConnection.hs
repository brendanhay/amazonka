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
-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection.
--
--
-- Deleting a connection only stops the AWS Direct Connect port hour and data transfer charges. If you are partnering with any third parties to connect with the AWS Direct Connect location, you must cancel your service with them separately.
--
module Network.AWS.DirectConnect.DeleteConnection
    (
    -- * Creating a Request
      deleteConnection
    , DeleteConnection
    -- * Request Lenses
    , dcConnectionId

    -- * Destructuring the Response
    , connection
    , Connection
    -- * Response Lenses
    , cLagId
    , cVlan
    , cLocation
    , cAwsDevice
    , cHasLogicalRedundancy
    , cConnectionId
    , cLoaIssueTime
    , cPartnerName
    , cConnectionName
    , cBandwidth
    , cJumboFrameCapable
    , cOwnerAccount
    , cRegion
    , cAwsDeviceV2
    , cConnectionState
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConnection' smart constructor.
newtype DeleteConnection = DeleteConnection'
  { _dcConnectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcConnectionId' - The ID of the connection.
deleteConnection
    :: Text -- ^ 'dcConnectionId'
    -> DeleteConnection
deleteConnection pConnectionId_ =
  DeleteConnection' {_dcConnectionId = pConnectionId_}


-- | The ID of the connection.
dcConnectionId :: Lens' DeleteConnection Text
dcConnectionId = lens _dcConnectionId (\ s a -> s{_dcConnectionId = a})

instance AWSRequest DeleteConnection where
        type Rs DeleteConnection = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DeleteConnection where

instance NFData DeleteConnection where

instance ToHeaders DeleteConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConnection where
        toJSON DeleteConnection'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _dcConnectionId)])

instance ToPath DeleteConnection where
        toPath = const "/"

instance ToQuery DeleteConnection where
        toQuery = const mempty
