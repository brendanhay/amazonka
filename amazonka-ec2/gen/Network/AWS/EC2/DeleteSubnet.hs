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
    , dellDryRun
    , dellSubnetId

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
-- * 'dellDryRun'
--
-- * 'dellSubnetId'
data DeleteSubnet = DeleteSubnet'
    { _dellDryRun   :: !(Maybe Bool)
    , _dellSubnetId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnet' smart constructor.
deleteSubnet :: Text -> DeleteSubnet
deleteSubnet pSubnetId =
    DeleteSubnet'
    { _dellDryRun = Nothing
    , _dellSubnetId = pSubnetId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dellDryRun :: Lens' DeleteSubnet (Maybe Bool)
dellDryRun = lens _dellDryRun (\ s a -> s{_dellDryRun = a});

-- | The ID of the subnet.
dellSubnetId :: Lens' DeleteSubnet Text
dellSubnetId = lens _dellSubnetId (\ s a -> s{_dellSubnetId = a});

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
               "DryRun" =: _dellDryRun, "SubnetId" =: _dellSubnetId]

-- | /See:/ 'deleteSubnetResponse' smart constructor.
data DeleteSubnetResponse =
    DeleteSubnetResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnetResponse' smart constructor.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse'
