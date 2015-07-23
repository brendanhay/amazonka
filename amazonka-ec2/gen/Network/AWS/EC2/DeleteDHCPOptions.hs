{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteDHCPOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set
-- of DHCP options before you can delete it. You can disassociate the set
-- of DHCP options by associating either a new set of options or the
-- default set of options with the VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteDHCPOptions.html>
module Network.AWS.EC2.DeleteDHCPOptions
    (
    -- * Request
      DeleteDHCPOptions
    -- ** Request constructor
    , deleteDHCPOptions
    -- ** Request lenses
    , ddhcporqDryRun
    , ddhcporqDHCPOptionsId

    -- * Response
    , DeleteDHCPOptionsResponse
    -- ** Response constructor
    , deleteDHCPOptionsResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteDHCPOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddhcporqDryRun'
--
-- * 'ddhcporqDHCPOptionsId'
data DeleteDHCPOptions = DeleteDHCPOptions'
    { _ddhcporqDryRun        :: !(Maybe Bool)
    , _ddhcporqDHCPOptionsId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDHCPOptions' smart constructor.
deleteDHCPOptions :: Text -> DeleteDHCPOptions
deleteDHCPOptions pDHCPOptionsId_ =
    DeleteDHCPOptions'
    { _ddhcporqDryRun = Nothing
    , _ddhcporqDHCPOptionsId = pDHCPOptionsId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ddhcporqDryRun :: Lens' DeleteDHCPOptions (Maybe Bool)
ddhcporqDryRun = lens _ddhcporqDryRun (\ s a -> s{_ddhcporqDryRun = a});

-- | The ID of the DHCP options set.
ddhcporqDHCPOptionsId :: Lens' DeleteDHCPOptions Text
ddhcporqDHCPOptionsId = lens _ddhcporqDHCPOptionsId (\ s a -> s{_ddhcporqDHCPOptionsId = a});

instance AWSRequest DeleteDHCPOptions where
        type Sv DeleteDHCPOptions = EC2
        type Rs DeleteDHCPOptions = DeleteDHCPOptionsResponse
        request = post
        response = receiveNull DeleteDHCPOptionsResponse'

instance ToHeaders DeleteDHCPOptions where
        toHeaders = const mempty

instance ToPath DeleteDHCPOptions where
        toPath = const "/"

instance ToQuery DeleteDHCPOptions where
        toQuery DeleteDHCPOptions'{..}
          = mconcat
              ["Action" =: ("DeleteDHCPOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ddhcporqDryRun,
               "DhcpOptionsId" =: _ddhcporqDHCPOptionsId]

-- | /See:/ 'deleteDHCPOptionsResponse' smart constructor.
data DeleteDHCPOptionsResponse =
    DeleteDHCPOptionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDHCPOptionsResponse' smart constructor.
deleteDHCPOptionsResponse :: DeleteDHCPOptionsResponse
deleteDHCPOptionsResponse = DeleteDHCPOptionsResponse'
