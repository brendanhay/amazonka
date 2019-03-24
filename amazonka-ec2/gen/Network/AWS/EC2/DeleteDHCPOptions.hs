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
-- Module      : Network.AWS.EC2.DeleteDHCPOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set of DHCP options before you can delete it. You can disassociate the set of DHCP options by associating either a new set of options or the default set of options with the VPC.
--
--
module Network.AWS.EC2.DeleteDHCPOptions
    (
    -- * Creating a Request
      deleteDHCPOptions
    , DeleteDHCPOptions
    -- * Request Lenses
    , ddhcpoDryRun
    , ddhcpoDHCPOptionsId

    -- * Destructuring the Response
    , deleteDHCPOptionsResponse
    , DeleteDHCPOptionsResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDHCPOptions' smart constructor.
data DeleteDHCPOptions = DeleteDHCPOptions'
  { _ddhcpoDryRun        :: !(Maybe Bool)
  , _ddhcpoDHCPOptionsId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddhcpoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ddhcpoDHCPOptionsId' - The ID of the DHCP options set.
deleteDHCPOptions
    :: Text -- ^ 'ddhcpoDHCPOptionsId'
    -> DeleteDHCPOptions
deleteDHCPOptions pDHCPOptionsId_ =
  DeleteDHCPOptions'
    {_ddhcpoDryRun = Nothing, _ddhcpoDHCPOptionsId = pDHCPOptionsId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ddhcpoDryRun :: Lens' DeleteDHCPOptions (Maybe Bool)
ddhcpoDryRun = lens _ddhcpoDryRun (\ s a -> s{_ddhcpoDryRun = a})

-- | The ID of the DHCP options set.
ddhcpoDHCPOptionsId :: Lens' DeleteDHCPOptions Text
ddhcpoDHCPOptionsId = lens _ddhcpoDHCPOptionsId (\ s a -> s{_ddhcpoDHCPOptionsId = a})

instance AWSRequest DeleteDHCPOptions where
        type Rs DeleteDHCPOptions = DeleteDHCPOptionsResponse
        request = postQuery ec2
        response = receiveNull DeleteDHCPOptionsResponse'

instance Hashable DeleteDHCPOptions where

instance NFData DeleteDHCPOptions where

instance ToHeaders DeleteDHCPOptions where
        toHeaders = const mempty

instance ToPath DeleteDHCPOptions where
        toPath = const "/"

instance ToQuery DeleteDHCPOptions where
        toQuery DeleteDHCPOptions'{..}
          = mconcat
              ["Action" =: ("DeleteDhcpOptions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ddhcpoDryRun,
               "DhcpOptionsId" =: _ddhcpoDHCPOptionsId]

-- | /See:/ 'deleteDHCPOptionsResponse' smart constructor.
data DeleteDHCPOptionsResponse =
  DeleteDHCPOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDHCPOptionsResponse' with the minimum fields required to make a request.
--
deleteDHCPOptionsResponse
    :: DeleteDHCPOptionsResponse
deleteDHCPOptionsResponse = DeleteDHCPOptionsResponse'


instance NFData DeleteDHCPOptionsResponse where
