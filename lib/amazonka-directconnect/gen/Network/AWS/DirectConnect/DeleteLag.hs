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
-- Module      : Network.AWS.DirectConnect.DeleteLag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.
--
--
module Network.AWS.DirectConnect.DeleteLag
    (
    -- * Creating a Request
      deleteLag
    , DeleteLag
    -- * Request Lenses
    , dLagId

    -- * Destructuring the Response
    , lag
    , Lag
    -- * Response Lenses
    , lagLagId
    , lagConnectionsBandwidth
    , lagMinimumLinks
    , lagLagName
    , lagLocation
    , lagConnections
    , lagAwsDevice
    , lagAllowsHostedConnections
    , lagNumberOfConnections
    , lagLagState
    , lagOwnerAccount
    , lagRegion
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DeleteLag operation.
--
--
--
-- /See:/ 'deleteLag' smart constructor.
newtype DeleteLag = DeleteLag'
  { _dLagId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLagId' - The ID of the LAG to delete. Example: dxlag-abc123 Default: None
deleteLag
    :: Text -- ^ 'dLagId'
    -> DeleteLag
deleteLag pLagId_ = DeleteLag' {_dLagId = pLagId_}


-- | The ID of the LAG to delete. Example: dxlag-abc123 Default: None
dLagId :: Lens' DeleteLag Text
dLagId = lens _dLagId (\ s a -> s{_dLagId = a})

instance AWSRequest DeleteLag where
        type Rs DeleteLag = Lag
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DeleteLag where

instance NFData DeleteLag where

instance ToHeaders DeleteLag where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteLag" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLag where
        toJSON DeleteLag'{..}
          = object (catMaybes [Just ("lagId" .= _dLagId)])

instance ToPath DeleteLag where
        toPath = const "/"

instance ToQuery DeleteLag where
        toQuery = const mempty
