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
-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new connection between the customer network and a specific AWS Direct Connect location.
--
-- A connection links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. An AWS Direct Connect location provides access to Amazon Web Services in the region it is associated with. You can establish connections with AWS Direct Connect locations in multiple regions, but a connection in one region does not provide connectivity to other regions.
module Network.AWS.DirectConnect.CreateConnection
    (
    -- * Creating a Request
      createConnection
    , CreateConnection
    -- * Request Lenses
    , ccLocation
    , ccBandwidth
    , ccConnectionName

    -- * Destructuring the Response
    , connection
    , Connection
    -- * Response Lenses
    , cVlan
    , cLocation
    , cConnectionId
    , cLoaIssueTime
    , cPartnerName
    , cConnectionName
    , cBandwidth
    , cOwnerAccount
    , cRegion
    , cConnectionState
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the CreateConnection operation.
--
-- /See:/ 'createConnection' smart constructor.
data CreateConnection = CreateConnection'
    { _ccLocation       :: !Text
    , _ccBandwidth      :: !Text
    , _ccConnectionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccLocation'
--
-- * 'ccBandwidth'
--
-- * 'ccConnectionName'
createConnection
    :: Text -- ^ 'ccLocation'
    -> Text -- ^ 'ccBandwidth'
    -> Text -- ^ 'ccConnectionName'
    -> CreateConnection
createConnection pLocation_ pBandwidth_ pConnectionName_ =
    CreateConnection'
    { _ccLocation = pLocation_
    , _ccBandwidth = pBandwidth_
    , _ccConnectionName = pConnectionName_
    }

-- | Undocumented member.
ccLocation :: Lens' CreateConnection Text
ccLocation = lens _ccLocation (\ s a -> s{_ccLocation = a});

-- | Undocumented member.
ccBandwidth :: Lens' CreateConnection Text
ccBandwidth = lens _ccBandwidth (\ s a -> s{_ccBandwidth = a});

-- | Undocumented member.
ccConnectionName :: Lens' CreateConnection Text
ccConnectionName = lens _ccConnectionName (\ s a -> s{_ccConnectionName = a});

instance AWSRequest CreateConnection where
        type Rs CreateConnection = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateConnection

instance NFData CreateConnection

instance ToHeaders CreateConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConnection where
        toJSON CreateConnection'{..}
          = object
              (catMaybes
                 [Just ("location" .= _ccLocation),
                  Just ("bandwidth" .= _ccBandwidth),
                  Just ("connectionName" .= _ccConnectionName)])

instance ToPath CreateConnection where
        toPath = const "/"

instance ToQuery CreateConnection where
        toQuery = const mempty
