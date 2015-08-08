{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can\'t delete the ACL if it\'s
-- associated with any subnets. You can\'t delete the default network ACL.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkACL.html AWS API Reference> for DeleteNetworkACL.
module Network.AWS.EC2.DeleteNetworkACL
    (
    -- * Creating a Request
      DeleteNetworkACL
    , deleteNetworkACL
    -- * Request Lenses
    , dnaDryRun
    , dnaNetworkACLId

    -- * Destructuring the Response
    , DeleteNetworkACLResponse
    , deleteNetworkACLResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteNetworkACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnaDryRun'
--
-- * 'dnaNetworkACLId'
data DeleteNetworkACL = DeleteNetworkACL'
    { _dnaDryRun       :: !(Maybe Bool)
    , _dnaNetworkACLId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteNetworkACL' smart constructor.
deleteNetworkACL :: Text -> DeleteNetworkACL
deleteNetworkACL pNetworkACLId_ =
    DeleteNetworkACL'
    { _dnaDryRun = Nothing
    , _dnaNetworkACLId = pNetworkACLId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dnaDryRun :: Lens' DeleteNetworkACL (Maybe Bool)
dnaDryRun = lens _dnaDryRun (\ s a -> s{_dnaDryRun = a});

-- | The ID of the network ACL.
dnaNetworkACLId :: Lens' DeleteNetworkACL Text
dnaNetworkACLId = lens _dnaNetworkACLId (\ s a -> s{_dnaNetworkACLId = a});

instance AWSRequest DeleteNetworkACL where
        type Sv DeleteNetworkACL = EC2
        type Rs DeleteNetworkACL = DeleteNetworkACLResponse
        request = post
        response = receiveNull DeleteNetworkACLResponse'

instance ToHeaders DeleteNetworkACL where
        toHeaders = const mempty

instance ToPath DeleteNetworkACL where
        toPath = const "/"

instance ToQuery DeleteNetworkACL where
        toQuery DeleteNetworkACL'{..}
          = mconcat
              ["Action" =: ("DeleteNetworkAcl" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dnaDryRun,
               "NetworkAclId" =: _dnaNetworkACLId]

-- | /See:/ 'deleteNetworkACLResponse' smart constructor.
data DeleteNetworkACLResponse =
    DeleteNetworkACLResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteNetworkACLResponse' smart constructor.
deleteNetworkACLResponse :: DeleteNetworkACLResponse
deleteNetworkACLResponse = DeleteNetworkACLResponse'
