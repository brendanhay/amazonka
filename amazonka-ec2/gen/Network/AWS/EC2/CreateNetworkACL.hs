{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkACL.html>
module Network.AWS.EC2.CreateNetworkACL
    (
    -- * Request
      CreateNetworkACL
    -- ** Request constructor
    , createNetworkACL
    -- ** Request lenses
    , cnarqDryRun
    , cnarqVPCId

    -- * Response
    , CreateNetworkACLResponse
    -- ** Response constructor
    , createNetworkACLResponse
    -- ** Response lenses
    , cnarsNetworkACL
    , cnarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createNetworkACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnarqDryRun'
--
-- * 'cnarqVPCId'
data CreateNetworkACL = CreateNetworkACL'
    { _cnarqDryRun :: !(Maybe Bool)
    , _cnarqVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACL' smart constructor.
createNetworkACL :: Text -> CreateNetworkACL
createNetworkACL pVPCId =
    CreateNetworkACL'
    { _cnarqDryRun = Nothing
    , _cnarqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cnarqDryRun :: Lens' CreateNetworkACL (Maybe Bool)
cnarqDryRun = lens _cnarqDryRun (\ s a -> s{_cnarqDryRun = a});

-- | The ID of the VPC.
cnarqVPCId :: Lens' CreateNetworkACL Text
cnarqVPCId = lens _cnarqVPCId (\ s a -> s{_cnarqVPCId = a});

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
              ["Action" =: ("CreateNetworkACL" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cnarqDryRun, "VpcId" =: _cnarqVPCId]

-- | /See:/ 'createNetworkACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnarsNetworkACL'
--
-- * 'cnarsStatus'
data CreateNetworkACLResponse = CreateNetworkACLResponse'
    { _cnarsNetworkACL :: !(Maybe NetworkACL)
    , _cnarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkACLResponse' smart constructor.
createNetworkACLResponse :: Int -> CreateNetworkACLResponse
createNetworkACLResponse pStatus =
    CreateNetworkACLResponse'
    { _cnarsNetworkACL = Nothing
    , _cnarsStatus = pStatus
    }

-- | Information about the network ACL.
cnarsNetworkACL :: Lens' CreateNetworkACLResponse (Maybe NetworkACL)
cnarsNetworkACL = lens _cnarsNetworkACL (\ s a -> s{_cnarsNetworkACL = a});

-- | FIXME: Undocumented member.
cnarsStatus :: Lens' CreateNetworkACLResponse Int
cnarsStatus = lens _cnarsStatus (\ s a -> s{_cnarsStatus = a});
