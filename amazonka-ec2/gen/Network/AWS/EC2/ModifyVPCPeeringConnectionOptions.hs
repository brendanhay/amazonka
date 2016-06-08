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
-- Module      : Network.AWS.EC2.ModifyVPCPeeringConnectionOptions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPC peering connection options on one side of a VPC peering connection. You can do the following:
--
-- -   Enable\/disable communication over the peering connection between an EC2-Classic instance that\'s linked to your VPC (using ClassicLink) and instances in the peer VPC.
--
-- -   Enable\/disable communication over the peering connection between instances in your VPC and an EC2-Classic instance that\'s linked to the peer VPC.
--
-- If the peered VPCs are in different accounts, each owner must initiate a separate request to enable or disable communication in either direction, depending on whether their VPC was the requester or accepter for the VPC peering connection. If the peered VPCs are in the same account, you can modify the requester and accepter options in the same request. To confirm which VPC is the accepter and requester for a VPC peering connection, use the < DescribeVpcPeeringConnections> command.
module Network.AWS.EC2.ModifyVPCPeeringConnectionOptions
    (
    -- * Creating a Request
      modifyVPCPeeringConnectionOptions
    , ModifyVPCPeeringConnectionOptions
    -- * Request Lenses
    , mvpcoRequesterPeeringConnectionOptions
    , mvpcoAccepterPeeringConnectionOptions
    , mvpcoDryRun
    , mvpcoVPCPeeringConnectionId

    -- * Destructuring the Response
    , modifyVPCPeeringConnectionOptionsResponse
    , ModifyVPCPeeringConnectionOptionsResponse
    -- * Response Lenses
    , mvpcorsRequesterPeeringConnectionOptions
    , mvpcorsAccepterPeeringConnectionOptions
    , mvpcorsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyVPCPeeringConnectionOptions' smart constructor.
data ModifyVPCPeeringConnectionOptions = ModifyVPCPeeringConnectionOptions'
    { _mvpcoRequesterPeeringConnectionOptions :: !(Maybe PeeringConnectionOptionsRequest)
    , _mvpcoAccepterPeeringConnectionOptions  :: !(Maybe PeeringConnectionOptionsRequest)
    , _mvpcoDryRun                            :: !(Maybe Bool)
    , _mvpcoVPCPeeringConnectionId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyVPCPeeringConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvpcoRequesterPeeringConnectionOptions'
--
-- * 'mvpcoAccepterPeeringConnectionOptions'
--
-- * 'mvpcoDryRun'
--
-- * 'mvpcoVPCPeeringConnectionId'
modifyVPCPeeringConnectionOptions
    :: Text -- ^ 'mvpcoVPCPeeringConnectionId'
    -> ModifyVPCPeeringConnectionOptions
modifyVPCPeeringConnectionOptions pVPCPeeringConnectionId_ =
    ModifyVPCPeeringConnectionOptions'
    { _mvpcoRequesterPeeringConnectionOptions = Nothing
    , _mvpcoAccepterPeeringConnectionOptions = Nothing
    , _mvpcoDryRun = Nothing
    , _mvpcoVPCPeeringConnectionId = pVPCPeeringConnectionId_
    }

-- | The VPC peering connection options for the requester VPC.
mvpcoRequesterPeeringConnectionOptions :: Lens' ModifyVPCPeeringConnectionOptions (Maybe PeeringConnectionOptionsRequest)
mvpcoRequesterPeeringConnectionOptions = lens _mvpcoRequesterPeeringConnectionOptions (\ s a -> s{_mvpcoRequesterPeeringConnectionOptions = a});

-- | The VPC peering connection options for the accepter VPC.
mvpcoAccepterPeeringConnectionOptions :: Lens' ModifyVPCPeeringConnectionOptions (Maybe PeeringConnectionOptionsRequest)
mvpcoAccepterPeeringConnectionOptions = lens _mvpcoAccepterPeeringConnectionOptions (\ s a -> s{_mvpcoAccepterPeeringConnectionOptions = a});

-- | Checks whether you have the required permissions for the operation, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
mvpcoDryRun :: Lens' ModifyVPCPeeringConnectionOptions (Maybe Bool)
mvpcoDryRun = lens _mvpcoDryRun (\ s a -> s{_mvpcoDryRun = a});

-- | The ID of the VPC peering connection.
mvpcoVPCPeeringConnectionId :: Lens' ModifyVPCPeeringConnectionOptions Text
mvpcoVPCPeeringConnectionId = lens _mvpcoVPCPeeringConnectionId (\ s a -> s{_mvpcoVPCPeeringConnectionId = a});

instance AWSRequest ModifyVPCPeeringConnectionOptions
         where
        type Rs ModifyVPCPeeringConnectionOptions =
             ModifyVPCPeeringConnectionOptionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCPeeringConnectionOptionsResponse' <$>
                   (x .@? "requesterPeeringConnectionOptions") <*>
                     (x .@? "accepterPeeringConnectionOptions")
                     <*> (pure (fromEnum s)))

instance Hashable ModifyVPCPeeringConnectionOptions

instance NFData ModifyVPCPeeringConnectionOptions

instance ToHeaders ModifyVPCPeeringConnectionOptions
         where
        toHeaders = const mempty

instance ToPath ModifyVPCPeeringConnectionOptions
         where
        toPath = const "/"

instance ToQuery ModifyVPCPeeringConnectionOptions
         where
        toQuery ModifyVPCPeeringConnectionOptions'{..}
          = mconcat
              ["Action" =:
                 ("ModifyVpcPeeringConnectionOptions" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "RequesterPeeringConnectionOptions" =:
                 _mvpcoRequesterPeeringConnectionOptions,
               "AccepterPeeringConnectionOptions" =:
                 _mvpcoAccepterPeeringConnectionOptions,
               "DryRun" =: _mvpcoDryRun,
               "VpcPeeringConnectionId" =:
                 _mvpcoVPCPeeringConnectionId]

-- | /See:/ 'modifyVPCPeeringConnectionOptionsResponse' smart constructor.
data ModifyVPCPeeringConnectionOptionsResponse = ModifyVPCPeeringConnectionOptionsResponse'
    { _mvpcorsRequesterPeeringConnectionOptions :: !(Maybe PeeringConnectionOptions)
    , _mvpcorsAccepterPeeringConnectionOptions  :: !(Maybe PeeringConnectionOptions)
    , _mvpcorsResponseStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyVPCPeeringConnectionOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvpcorsRequesterPeeringConnectionOptions'
--
-- * 'mvpcorsAccepterPeeringConnectionOptions'
--
-- * 'mvpcorsResponseStatus'
modifyVPCPeeringConnectionOptionsResponse
    :: Int -- ^ 'mvpcorsResponseStatus'
    -> ModifyVPCPeeringConnectionOptionsResponse
modifyVPCPeeringConnectionOptionsResponse pResponseStatus_ =
    ModifyVPCPeeringConnectionOptionsResponse'
    { _mvpcorsRequesterPeeringConnectionOptions = Nothing
    , _mvpcorsAccepterPeeringConnectionOptions = Nothing
    , _mvpcorsResponseStatus = pResponseStatus_
    }

-- | Information about the VPC peering connection options for the requester VPC.
mvpcorsRequesterPeeringConnectionOptions :: Lens' ModifyVPCPeeringConnectionOptionsResponse (Maybe PeeringConnectionOptions)
mvpcorsRequesterPeeringConnectionOptions = lens _mvpcorsRequesterPeeringConnectionOptions (\ s a -> s{_mvpcorsRequesterPeeringConnectionOptions = a});

-- | Information about the VPC peering connection options for the accepter VPC.
mvpcorsAccepterPeeringConnectionOptions :: Lens' ModifyVPCPeeringConnectionOptionsResponse (Maybe PeeringConnectionOptions)
mvpcorsAccepterPeeringConnectionOptions = lens _mvpcorsAccepterPeeringConnectionOptions (\ s a -> s{_mvpcorsAccepterPeeringConnectionOptions = a});

-- | The response status code.
mvpcorsResponseStatus :: Lens' ModifyVPCPeeringConnectionOptionsResponse Int
mvpcorsResponseStatus = lens _mvpcorsResponseStatus (\ s a -> s{_mvpcorsResponseStatus = a});

instance NFData
         ModifyVPCPeeringConnectionOptionsResponse
