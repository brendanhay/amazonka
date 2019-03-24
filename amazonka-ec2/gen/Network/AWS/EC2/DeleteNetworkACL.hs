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
-- Module      : Network.AWS.EC2.DeleteNetworkACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can't delete the ACL if it's associated with any subnets. You can't delete the default network ACL.
--
--
module Network.AWS.EC2.DeleteNetworkACL
    (
    -- * Creating a Request
      deleteNetworkACL
    , DeleteNetworkACL
    -- * Request Lenses
    , dnaDryRun
    , dnaNetworkACLId

    -- * Destructuring the Response
    , deleteNetworkACLResponse
    , DeleteNetworkACLResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNetworkACL' smart constructor.
data DeleteNetworkACL = DeleteNetworkACL'
  { _dnaDryRun       :: !(Maybe Bool)
  , _dnaNetworkACLId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dnaNetworkACLId' - The ID of the network ACL.
deleteNetworkACL
    :: Text -- ^ 'dnaNetworkACLId'
    -> DeleteNetworkACL
deleteNetworkACL pNetworkACLId_ =
  DeleteNetworkACL' {_dnaDryRun = Nothing, _dnaNetworkACLId = pNetworkACLId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnaDryRun :: Lens' DeleteNetworkACL (Maybe Bool)
dnaDryRun = lens _dnaDryRun (\ s a -> s{_dnaDryRun = a})

-- | The ID of the network ACL.
dnaNetworkACLId :: Lens' DeleteNetworkACL Text
dnaNetworkACLId = lens _dnaNetworkACLId (\ s a -> s{_dnaNetworkACLId = a})

instance AWSRequest DeleteNetworkACL where
        type Rs DeleteNetworkACL = DeleteNetworkACLResponse
        request = postQuery ec2
        response = receiveNull DeleteNetworkACLResponse'

instance Hashable DeleteNetworkACL where

instance NFData DeleteNetworkACL where

instance ToHeaders DeleteNetworkACL where
        toHeaders = const mempty

instance ToPath DeleteNetworkACL where
        toPath = const "/"

instance ToQuery DeleteNetworkACL where
        toQuery DeleteNetworkACL'{..}
          = mconcat
              ["Action" =: ("DeleteNetworkAcl" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dnaDryRun,
               "NetworkAclId" =: _dnaNetworkACLId]

-- | /See:/ 'deleteNetworkACLResponse' smart constructor.
data DeleteNetworkACLResponse =
  DeleteNetworkACLResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkACLResponse' with the minimum fields required to make a request.
--
deleteNetworkACLResponse
    :: DeleteNetworkACLResponse
deleteNetworkACLResponse = DeleteNetworkACLResponse'


instance NFData DeleteNetworkACLResponse where
