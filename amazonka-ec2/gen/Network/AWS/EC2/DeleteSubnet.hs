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
    , ds1DryRun
    , ds1SubnetId

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
-- * 'ds1DryRun'
--
-- * 'ds1SubnetId'
data DeleteSubnet = DeleteSubnet'
    { _ds1DryRun   :: !(Maybe Bool)
    , _ds1SubnetId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnet' smart constructor.
deleteSubnet :: Text -> DeleteSubnet
deleteSubnet pSubnetId =
    DeleteSubnet'
    { _ds1DryRun = Nothing
    , _ds1SubnetId = pSubnetId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ds1DryRun :: Lens' DeleteSubnet (Maybe Bool)
ds1DryRun = lens _ds1DryRun (\ s a -> s{_ds1DryRun = a});

-- | The ID of the subnet.
ds1SubnetId :: Lens' DeleteSubnet Text
ds1SubnetId = lens _ds1SubnetId (\ s a -> s{_ds1SubnetId = a});

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
               "DryRun" =: _ds1DryRun, "SubnetId" =: _ds1SubnetId]

-- | /See:/ 'deleteSubnetResponse' smart constructor.
data DeleteSubnetResponse =
    DeleteSubnetResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSubnetResponse' smart constructor.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse'
