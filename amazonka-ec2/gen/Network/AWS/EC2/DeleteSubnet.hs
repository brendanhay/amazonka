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
-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subnet. You must terminate all running instances in the subnet before you can delete the subnet.
--
--
module Network.AWS.EC2.DeleteSubnet
    (
    -- * Creating a Request
      deleteSubnet
    , DeleteSubnet
    -- * Request Lenses
    , ddDryRun
    , ddSubnetId

    -- * Destructuring the Response
    , deleteSubnetResponse
    , DeleteSubnetResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSubnet' smart constructor.
data DeleteSubnet = DeleteSubnet'
  { _ddDryRun   :: !(Maybe Bool)
  , _ddSubnetId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ddSubnetId' - The ID of the subnet.
deleteSubnet
    :: Text -- ^ 'ddSubnetId'
    -> DeleteSubnet
deleteSubnet pSubnetId_ =
  DeleteSubnet' {_ddDryRun = Nothing, _ddSubnetId = pSubnetId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ddDryRun :: Lens' DeleteSubnet (Maybe Bool)
ddDryRun = lens _ddDryRun (\ s a -> s{_ddDryRun = a})

-- | The ID of the subnet.
ddSubnetId :: Lens' DeleteSubnet Text
ddSubnetId = lens _ddSubnetId (\ s a -> s{_ddSubnetId = a})

instance AWSRequest DeleteSubnet where
        type Rs DeleteSubnet = DeleteSubnetResponse
        request = postQuery ec2
        response = receiveNull DeleteSubnetResponse'

instance Hashable DeleteSubnet where

instance NFData DeleteSubnet where

instance ToHeaders DeleteSubnet where
        toHeaders = const mempty

instance ToPath DeleteSubnet where
        toPath = const "/"

instance ToQuery DeleteSubnet where
        toQuery DeleteSubnet'{..}
          = mconcat
              ["Action" =: ("DeleteSubnet" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ddDryRun, "SubnetId" =: _ddSubnetId]

-- | /See:/ 'deleteSubnetResponse' smart constructor.
data DeleteSubnetResponse =
  DeleteSubnetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubnetResponse' with the minimum fields required to make a request.
--
deleteSubnetResponse
    :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse'


instance NFData DeleteSubnetResponse where
