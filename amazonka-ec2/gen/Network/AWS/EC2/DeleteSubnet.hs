{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified subnet. You must terminate all running instances
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
    , del1DryRun
    , del1SubnetId

    -- * Response
    , DeleteSubnetResponse
    -- ** Response constructor
    , deleteSubnetResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'deleteSubnet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'del1DryRun'
--
-- * 'del1SubnetId'
data DeleteSubnet = DeleteSubnet'{_del1DryRun :: Maybe Bool, _del1SubnetId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteSubnet' smart constructor.
deleteSubnet :: Text -> DeleteSubnet
deleteSubnet pSubnetId = DeleteSubnet'{_del1DryRun = Nothing, _del1SubnetId = pSubnetId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
del1DryRun :: Lens' DeleteSubnet (Maybe Bool)
del1DryRun = lens _del1DryRun (\ s a -> s{_del1DryRun = a});

-- | The ID of the subnet.
del1SubnetId :: Lens' DeleteSubnet Text
del1SubnetId = lens _del1SubnetId (\ s a -> s{_del1SubnetId = a});

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
               "DryRun" =: _del1DryRun, "SubnetId" =: _del1SubnetId]

-- | /See:/ 'deleteSubnetResponse' smart constructor.
data DeleteSubnetResponse = DeleteSubnetResponse' deriving (Eq, Read, Show)

-- | 'DeleteSubnetResponse' smart constructor.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse';
