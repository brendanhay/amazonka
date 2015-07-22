{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subnet. You must terminate all running instances
-- in the subnet before you can delete the subnet.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSubnet.html>
module Network.AWS.EC2.DeleteSubnet
    (
    -- * Request
      DeleteSubnet
    -- ** Request constructor
    , deleteSubnet
    -- ** Request lenses
    , ddrqDryRun
    , ddrqSubnetId

    -- * Response
    , DeleteSubnetResponse
    -- ** Response constructor
    , deleteSubnetResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSubnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrqDryRun'
--
-- * 'ddrqSubnetId'
data DeleteSubnet = DeleteSubnet'
    { _ddrqDryRun   :: !(Maybe Bool)
    , _ddrqSubnetId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnet' smart constructor.
deleteSubnet :: Text -> DeleteSubnet
deleteSubnet pSubnetId_ =
    DeleteSubnet'
    { _ddrqDryRun = Nothing
    , _ddrqSubnetId = pSubnetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ddrqDryRun :: Lens' DeleteSubnet (Maybe Bool)
ddrqDryRun = lens _ddrqDryRun (\ s a -> s{_ddrqDryRun = a});

-- | The ID of the subnet.
ddrqSubnetId :: Lens' DeleteSubnet Text
ddrqSubnetId = lens _ddrqSubnetId (\ s a -> s{_ddrqSubnetId = a});

instance AWSRequest DeleteSubnet where
        type Sv DeleteSubnet = EC2
        type Rs DeleteSubnet = DeleteSubnetResponse
        request = post
        response = receiveNull DeleteSubnetResponse'

instance ToHeaders DeleteSubnet where
        toHeaders = const mempty

instance ToPath DeleteSubnet where
        toPath = const "/"

instance ToQuery DeleteSubnet where
        toQuery DeleteSubnet'{..}
          = mconcat
              ["Action" =: ("DeleteSubnet" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ddrqDryRun, "SubnetId" =: _ddrqSubnetId]

-- | /See:/ 'deleteSubnetResponse' smart constructor.
data DeleteSubnetResponse =
    DeleteSubnetResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnetResponse' smart constructor.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse'
