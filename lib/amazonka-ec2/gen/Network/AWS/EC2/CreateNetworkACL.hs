{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer of security (in addition to security groups) for the instances in your VPC.
--
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNetworkACL
  ( -- * Creating a Request
    createNetworkACL,
    CreateNetworkACL,

    -- * Request Lenses
    cnaTagSpecifications,
    cnaDryRun,
    cnaVPCId,

    -- * Destructuring the Response
    createNetworkACLResponse,
    CreateNetworkACLResponse,

    -- * Response Lenses
    cnarsNetworkACL,
    cnarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createNetworkACL' smart constructor.
data CreateNetworkACL = CreateNetworkACL'
  { _cnaTagSpecifications ::
      !(Maybe [TagSpecification]),
    _cnaDryRun :: !(Maybe Bool),
    _cnaVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNetworkACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnaTagSpecifications' - The tags to assign to the network ACL.
--
-- * 'cnaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cnaVPCId' - The ID of the VPC.
createNetworkACL ::
  -- | 'cnaVPCId'
  Text ->
  CreateNetworkACL
createNetworkACL pVPCId_ =
  CreateNetworkACL'
    { _cnaTagSpecifications = Nothing,
      _cnaDryRun = Nothing,
      _cnaVPCId = pVPCId_
    }

-- | The tags to assign to the network ACL.
cnaTagSpecifications :: Lens' CreateNetworkACL [TagSpecification]
cnaTagSpecifications = lens _cnaTagSpecifications (\s a -> s {_cnaTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cnaDryRun :: Lens' CreateNetworkACL (Maybe Bool)
cnaDryRun = lens _cnaDryRun (\s a -> s {_cnaDryRun = a})

-- | The ID of the VPC.
cnaVPCId :: Lens' CreateNetworkACL Text
cnaVPCId = lens _cnaVPCId (\s a -> s {_cnaVPCId = a})

instance AWSRequest CreateNetworkACL where
  type Rs CreateNetworkACL = CreateNetworkACLResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateNetworkACLResponse'
            <$> (x .@? "networkAcl") <*> (pure (fromEnum s))
      )

instance Hashable CreateNetworkACL

instance NFData CreateNetworkACL

instance ToHeaders CreateNetworkACL where
  toHeaders = const mempty

instance ToPath CreateNetworkACL where
  toPath = const "/"

instance ToQuery CreateNetworkACL where
  toQuery CreateNetworkACL' {..} =
    mconcat
      [ "Action" =: ("CreateNetworkAcl" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "TagSpecification" <$> _cnaTagSpecifications),
        "DryRun" =: _cnaDryRun,
        "VpcId" =: _cnaVPCId
      ]

-- | /See:/ 'createNetworkACLResponse' smart constructor.
data CreateNetworkACLResponse = CreateNetworkACLResponse'
  { _cnarsNetworkACL ::
      !(Maybe NetworkACL),
    _cnarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNetworkACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnarsNetworkACL' - Information about the network ACL.
--
-- * 'cnarsResponseStatus' - -- | The response status code.
createNetworkACLResponse ::
  -- | 'cnarsResponseStatus'
  Int ->
  CreateNetworkACLResponse
createNetworkACLResponse pResponseStatus_ =
  CreateNetworkACLResponse'
    { _cnarsNetworkACL = Nothing,
      _cnarsResponseStatus = pResponseStatus_
    }

-- | Information about the network ACL.
cnarsNetworkACL :: Lens' CreateNetworkACLResponse (Maybe NetworkACL)
cnarsNetworkACL = lens _cnarsNetworkACL (\s a -> s {_cnarsNetworkACL = a})

-- | -- | The response status code.
cnarsResponseStatus :: Lens' CreateNetworkACLResponse Int
cnarsResponseStatus = lens _cnarsResponseStatus (\s a -> s {_cnarsResponseStatus = a})

instance NFData CreateNetworkACLResponse
