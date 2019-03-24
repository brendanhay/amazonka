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
-- Module      : Network.AWS.DirectConnect.UpdateLag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the attributes of the specified link aggregation group (LAG).
--
--
-- You can update the following attributes:
--
--     * The name of the LAG.
--
--     * The value for the minimum number of connections that must be operational for the LAG itself to be operational.
--
--
--
-- When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value and the number of operational connections falls below the specified value, the LAG automatically goes down to avoid over-utilization of the remaining connections. Adjust this value with care, as it could force the LAG down if it is set higher than the current number of operational connections.
--
module Network.AWS.DirectConnect.UpdateLag
    (
    -- * Creating a Request
      updateLag
    , UpdateLag
    -- * Request Lenses
    , ulMinimumLinks
    , ulLagName
    , ulLagId

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
    , lagHasLogicalRedundancy
    , lagAllowsHostedConnections
    , lagNumberOfConnections
    , lagJumboFrameCapable
    , lagLagState
    , lagOwnerAccount
    , lagRegion
    , lagAwsDeviceV2
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLag' smart constructor.
data UpdateLag = UpdateLag'
  { _ulMinimumLinks :: !(Maybe Int)
  , _ulLagName      :: !(Maybe Text)
  , _ulLagId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulMinimumLinks' - The minimum number of physical connections that must be operational for the LAG itself to be operational.
--
-- * 'ulLagName' - The name of the LAG.
--
-- * 'ulLagId' - The ID of the LAG.
updateLag
    :: Text -- ^ 'ulLagId'
    -> UpdateLag
updateLag pLagId_ =
  UpdateLag'
    {_ulMinimumLinks = Nothing, _ulLagName = Nothing, _ulLagId = pLagId_}


-- | The minimum number of physical connections that must be operational for the LAG itself to be operational.
ulMinimumLinks :: Lens' UpdateLag (Maybe Int)
ulMinimumLinks = lens _ulMinimumLinks (\ s a -> s{_ulMinimumLinks = a})

-- | The name of the LAG.
ulLagName :: Lens' UpdateLag (Maybe Text)
ulLagName = lens _ulLagName (\ s a -> s{_ulLagName = a})

-- | The ID of the LAG.
ulLagId :: Lens' UpdateLag Text
ulLagId = lens _ulLagId (\ s a -> s{_ulLagId = a})

instance AWSRequest UpdateLag where
        type Rs UpdateLag = Lag
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateLag where

instance NFData UpdateLag where

instance ToHeaders UpdateLag where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.UpdateLag" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateLag where
        toJSON UpdateLag'{..}
          = object
              (catMaybes
                 [("minimumLinks" .=) <$> _ulMinimumLinks,
                  ("lagName" .=) <$> _ulLagName,
                  Just ("lagId" .= _ulLagId)])

instance ToPath UpdateLag where
        toPath = const "/"

instance ToQuery UpdateLag where
        toQuery = const mempty
