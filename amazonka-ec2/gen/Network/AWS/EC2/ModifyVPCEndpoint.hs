{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. You can modify the
-- policy associated with the endpoint, and you can add and remove route
-- tables associated with the endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVPCEndpoint.html>
module Network.AWS.EC2.ModifyVPCEndpoint
    (
    -- * Request
      ModifyVPCEndpoint
    -- ** Request constructor
    , modifyVPCEndpoint
    -- ** Request lenses
    , mverqPolicyDocument
    , mverqRemoveRouteTableIds
    , mverqResetPolicy
    , mverqAddRouteTableIds
    , mverqDryRun
    , mverqVPCEndpointId

    -- * Response
    , ModifyVPCEndpointResponse
    -- ** Response constructor
    , modifyVPCEndpointResponse
    -- ** Response lenses
    , mversReturn
    , mversStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyVPCEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mverqPolicyDocument'
--
-- * 'mverqRemoveRouteTableIds'
--
-- * 'mverqResetPolicy'
--
-- * 'mverqAddRouteTableIds'
--
-- * 'mverqDryRun'
--
-- * 'mverqVPCEndpointId'
data ModifyVPCEndpoint = ModifyVPCEndpoint'
    { _mverqPolicyDocument      :: !(Maybe Text)
    , _mverqRemoveRouteTableIds :: !(Maybe [Text])
    , _mverqResetPolicy         :: !(Maybe Bool)
    , _mverqAddRouteTableIds    :: !(Maybe [Text])
    , _mverqDryRun              :: !(Maybe Bool)
    , _mverqVPCEndpointId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCEndpoint' smart constructor.
modifyVPCEndpoint :: Text -> ModifyVPCEndpoint
modifyVPCEndpoint pVPCEndpointId =
    ModifyVPCEndpoint'
    { _mverqPolicyDocument = Nothing
    , _mverqRemoveRouteTableIds = Nothing
    , _mverqResetPolicy = Nothing
    , _mverqAddRouteTableIds = Nothing
    , _mverqDryRun = Nothing
    , _mverqVPCEndpointId = pVPCEndpointId
    }

-- | A policy document to attach to the endpoint. The policy must be in valid
-- JSON format.
mverqPolicyDocument :: Lens' ModifyVPCEndpoint (Maybe Text)
mverqPolicyDocument = lens _mverqPolicyDocument (\ s a -> s{_mverqPolicyDocument = a});

-- | One or more route table IDs to disassociate from the endpoint.
mverqRemoveRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mverqRemoveRouteTableIds = lens _mverqRemoveRouteTableIds (\ s a -> s{_mverqRemoveRouteTableIds = a}) . _Default;

-- | Specify @true@ to reset the policy document to the default policy. The
-- default policy allows access to the service.
mverqResetPolicy :: Lens' ModifyVPCEndpoint (Maybe Bool)
mverqResetPolicy = lens _mverqResetPolicy (\ s a -> s{_mverqResetPolicy = a});

-- | One or more route tables IDs to associate with the endpoint.
mverqAddRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mverqAddRouteTableIds = lens _mverqAddRouteTableIds (\ s a -> s{_mverqAddRouteTableIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mverqDryRun :: Lens' ModifyVPCEndpoint (Maybe Bool)
mverqDryRun = lens _mverqDryRun (\ s a -> s{_mverqDryRun = a});

-- | The ID of the endpoint.
mverqVPCEndpointId :: Lens' ModifyVPCEndpoint Text
mverqVPCEndpointId = lens _mverqVPCEndpointId (\ s a -> s{_mverqVPCEndpointId = a});

instance AWSRequest ModifyVPCEndpoint where
        type Sv ModifyVPCEndpoint = EC2
        type Rs ModifyVPCEndpoint = ModifyVPCEndpointResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCEndpointResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders ModifyVPCEndpoint where
        toHeaders = const mempty

instance ToPath ModifyVPCEndpoint where
        toPath = const "/"

instance ToQuery ModifyVPCEndpoint where
        toQuery ModifyVPCEndpoint'{..}
          = mconcat
              ["Action" =: ("ModifyVPCEndpoint" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "PolicyDocument" =: _mverqPolicyDocument,
               toQuery
                 (toQueryList "item" <$> _mverqRemoveRouteTableIds),
               "ResetPolicy" =: _mverqResetPolicy,
               toQuery
                 (toQueryList "item" <$> _mverqAddRouteTableIds),
               "DryRun" =: _mverqDryRun,
               "VpcEndpointId" =: _mverqVPCEndpointId]

-- | /See:/ 'modifyVPCEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mversReturn'
--
-- * 'mversStatus'
data ModifyVPCEndpointResponse = ModifyVPCEndpointResponse'
    { _mversReturn :: !(Maybe Bool)
    , _mversStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyVPCEndpointResponse' smart constructor.
modifyVPCEndpointResponse :: Int -> ModifyVPCEndpointResponse
modifyVPCEndpointResponse pStatus =
    ModifyVPCEndpointResponse'
    { _mversReturn = Nothing
    , _mversStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mversReturn :: Lens' ModifyVPCEndpointResponse (Maybe Bool)
mversReturn = lens _mversReturn (\ s a -> s{_mversReturn = a});

-- | FIXME: Undocumented member.
mversStatus :: Lens' ModifyVPCEndpointResponse Int
mversStatus = lens _mversStatus (\ s a -> s{_mversStatus = a});
