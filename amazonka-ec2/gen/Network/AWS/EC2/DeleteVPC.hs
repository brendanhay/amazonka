{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and
-- so on.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPC.html>
module Network.AWS.EC2.DeleteVPC
    (
    -- * Request
      DeleteVPC
    -- ** Request constructor
    , deleteVPC
    -- ** Request lenses
    , delrqDryRun
    , delrqVPCId

    -- * Response
    , DeleteVPCResponse
    -- ** Response constructor
    , deleteVPCResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrqDryRun'
--
-- * 'delrqVPCId'
data DeleteVPC = DeleteVPC'
    { _delrqDryRun :: !(Maybe Bool)
    , _delrqVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPC' smart constructor.
deleteVPC :: Text -> DeleteVPC
deleteVPC pVPCId_ =
    DeleteVPC'
    { _delrqDryRun = Nothing
    , _delrqVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
delrqDryRun :: Lens' DeleteVPC (Maybe Bool)
delrqDryRun = lens _delrqDryRun (\ s a -> s{_delrqDryRun = a});

-- | The ID of the VPC.
delrqVPCId :: Lens' DeleteVPC Text
delrqVPCId = lens _delrqVPCId (\ s a -> s{_delrqVPCId = a});

instance AWSRequest DeleteVPC where
        type Sv DeleteVPC = EC2
        type Rs DeleteVPC = DeleteVPCResponse
        request = post
        response = receiveNull DeleteVPCResponse'

instance ToHeaders DeleteVPC where
        toHeaders = const mempty

instance ToPath DeleteVPC where
        toPath = const "/"

instance ToQuery DeleteVPC where
        toQuery DeleteVPC'{..}
          = mconcat
              ["Action" =: ("DeleteVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _delrqDryRun, "VpcId" =: _delrqVPCId]

-- | /See:/ 'deleteVPCResponse' smart constructor.
data DeleteVPCResponse =
    DeleteVPCResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPCResponse' smart constructor.
deleteVPCResponse :: DeleteVPCResponse
deleteVPCResponse = DeleteVPCResponse'
