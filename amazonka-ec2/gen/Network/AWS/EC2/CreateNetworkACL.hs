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
-- Module      : Network.AWS.EC2.CreateNetworkACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer
-- of security (in addition to security groups) for the instances in your
-- VPC.
--
-- For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkACL.html AWS API Reference> for CreateNetworkACL.
module Network.AWS.EC2.CreateNetworkACL
    (
    -- * Creating a Request
      createNetworkACL
    , CreateNetworkACL
    -- * Request Lenses
    , cnaDryRun
    , cnaVPCId

    -- * Destructuring the Response
    , createNetworkACLResponse
    , CreateNetworkACLResponse
    -- * Response Lenses
    , cnarsNetworkACL
    , cnarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createNetworkACL' smart constructor.
data CreateNetworkACL = CreateNetworkACL'
    { _cnaDryRun :: !(Maybe Bool)
    , _cnaVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNetworkACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnaDryRun'
--
-- * 'cnaVPCId'
createNetworkACL
    :: Text -- ^ 'cnaVPCId'
    -> CreateNetworkACL
createNetworkACL pVPCId_ =
    CreateNetworkACL'
    { _cnaDryRun = Nothing
    , _cnaVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
cnaDryRun :: Lens' CreateNetworkACL (Maybe Bool)
cnaDryRun = lens _cnaDryRun (\ s a -> s{_cnaDryRun = a});

-- | The ID of the VPC.
cnaVPCId :: Lens' CreateNetworkACL Text
cnaVPCId = lens _cnaVPCId (\ s a -> s{_cnaVPCId = a});

instance AWSRequest CreateNetworkACL where
        type Sv CreateNetworkACL = EC2
        type Rs CreateNetworkACL = CreateNetworkACLResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateNetworkACLResponse' <$>
                   (x .@? "networkAcl") <*> (pure (fromEnum s)))

instance ToHeaders CreateNetworkACL where
        toHeaders = const mempty

instance ToPath CreateNetworkACL where
        toPath = const "/"

instance ToQuery CreateNetworkACL where
        toQuery CreateNetworkACL'{..}
          = mconcat
              ["Action" =: ("CreateNetworkAcl" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cnaDryRun, "VpcId" =: _cnaVPCId]

-- | /See:/ 'createNetworkACLResponse' smart constructor.
data CreateNetworkACLResponse = CreateNetworkACLResponse'
    { _cnarsNetworkACL :: !(Maybe NetworkACL)
    , _cnarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNetworkACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnarsNetworkACL'
--
-- * 'cnarsStatus'
createNetworkACLResponse
    :: Int -- ^ 'cnarsStatus'
    -> CreateNetworkACLResponse
createNetworkACLResponse pStatus_ =
    CreateNetworkACLResponse'
    { _cnarsNetworkACL = Nothing
    , _cnarsStatus = pStatus_
    }

-- | Information about the network ACL.
cnarsNetworkACL :: Lens' CreateNetworkACLResponse (Maybe NetworkACL)
cnarsNetworkACL = lens _cnarsNetworkACL (\ s a -> s{_cnarsNetworkACL = a});

-- | The response status code.
cnarsStatus :: Lens' CreateNetworkACLResponse Int
cnarsStatus = lens _cnarsStatus (\ s a -> s{_cnarsStatus = a});
