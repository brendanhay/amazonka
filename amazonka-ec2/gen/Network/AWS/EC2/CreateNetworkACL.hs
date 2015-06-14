{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateNetworkACL
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

-- | Creates a network ACL in a VPC. Network ACLs provide an optional layer
-- of security (in addition to security groups) for the instances in your
-- VPC.
--
-- For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkACL.html>
module Network.AWS.EC2.CreateNetworkACL
    (
    -- * Request
      CreateNetworkACL
    -- ** Request constructor
    , createNetworkACL
    -- ** Request lenses
    , cnaDryRun
    , cnaVPCId

    -- * Response
    , CreateNetworkACLResponse
    -- ** Response constructor
    , createNetworkACLResponse
    -- ** Response lenses
    , cnarNetworkACL
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'createNetworkACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnaDryRun'
--
-- * 'cnaVPCId'
data CreateNetworkACL = CreateNetworkACL'{_cnaDryRun :: Maybe Bool, _cnaVPCId :: Text} deriving (Eq, Read, Show)

-- | 'CreateNetworkACL' smart constructor.
createNetworkACL :: Text -> CreateNetworkACL
createNetworkACL pVPCId = CreateNetworkACL'{_cnaDryRun = Nothing, _cnaVPCId = pVPCId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
                 CreateNetworkACLResponse' <$> x .@? "networkAcl")

instance ToHeaders CreateNetworkACL where
        toHeaders = const mempty

instance ToPath CreateNetworkACL where
        toPath = const "/"

instance ToQuery CreateNetworkACL where
        toQuery CreateNetworkACL'{..}
          = mconcat
              ["Action" =: ("CreateNetworkACL" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cnaDryRun, "VpcId" =: _cnaVPCId]

-- | /See:/ 'createNetworkACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnarNetworkACL'
newtype CreateNetworkACLResponse = CreateNetworkACLResponse'{_cnarNetworkACL :: Maybe NetworkACL} deriving (Eq, Read, Show)

-- | 'CreateNetworkACLResponse' smart constructor.
createNetworkACLResponse :: CreateNetworkACLResponse
createNetworkACLResponse = CreateNetworkACLResponse'{_cnarNetworkACL = Nothing};

-- | Information about the network ACL.
cnarNetworkACL :: Lens' CreateNetworkACLResponse (Maybe NetworkACL)
cnarNetworkACL = lens _cnarNetworkACL (\ s a -> s{_cnarNetworkACL = a});
