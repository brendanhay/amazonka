{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.ModifyVPCEndpoint
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

-- | Modifies attributes of a specified VPC endpoint. You can modify the
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
    , mvePolicyDocument
    , mveRemoveRouteTableIds
    , mveResetPolicy
    , mveAddRouteTableIds
    , mveDryRun
    , mveVPCEndpointId

    -- * Response
    , ModifyVPCEndpointResponse
    -- ** Response constructor
    , modifyVPCEndpointResponse
    -- ** Response lenses
    , mverReturn
    , mverStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyVPCEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mvePolicyDocument'
--
-- * 'mveRemoveRouteTableIds'
--
-- * 'mveResetPolicy'
--
-- * 'mveAddRouteTableIds'
--
-- * 'mveDryRun'
--
-- * 'mveVPCEndpointId'
data ModifyVPCEndpoint = ModifyVPCEndpoint'
    { _mvePolicyDocument      :: Maybe Text
    , _mveRemoveRouteTableIds :: Maybe [Text]
    , _mveResetPolicy         :: Maybe Bool
    , _mveAddRouteTableIds    :: Maybe [Text]
    , _mveDryRun              :: Maybe Bool
    , _mveVPCEndpointId       :: Text
    } deriving (Eq,Read,Show)

-- | 'ModifyVPCEndpoint' smart constructor.
modifyVPCEndpoint :: Text -> ModifyVPCEndpoint
modifyVPCEndpoint pVPCEndpointId =
    ModifyVPCEndpoint'
    { _mvePolicyDocument = Nothing
    , _mveRemoveRouteTableIds = Nothing
    , _mveResetPolicy = Nothing
    , _mveAddRouteTableIds = Nothing
    , _mveDryRun = Nothing
    , _mveVPCEndpointId = pVPCEndpointId
    }

-- | A policy document to attach to the endpoint. The policy must be in valid
-- JSON format.
mvePolicyDocument :: Lens' ModifyVPCEndpoint (Maybe Text)
mvePolicyDocument = lens _mvePolicyDocument (\ s a -> s{_mvePolicyDocument = a});

-- | One or more route table IDs to disassociate from the endpoint.
mveRemoveRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveRemoveRouteTableIds = lens _mveRemoveRouteTableIds (\ s a -> s{_mveRemoveRouteTableIds = a}) . _Default;

-- | Specify @true@ to reset the policy document to the default policy. The
-- default policy allows access to the service.
mveResetPolicy :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveResetPolicy = lens _mveResetPolicy (\ s a -> s{_mveResetPolicy = a});

-- | One or more route tables IDs to associate with the endpoint.
mveAddRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveAddRouteTableIds = lens _mveAddRouteTableIds (\ s a -> s{_mveAddRouteTableIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mveDryRun :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveDryRun = lens _mveDryRun (\ s a -> s{_mveDryRun = a});

-- | The ID of the endpoint.
mveVPCEndpointId :: Lens' ModifyVPCEndpoint Text
mveVPCEndpointId = lens _mveVPCEndpointId (\ s a -> s{_mveVPCEndpointId = a});

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
               "PolicyDocument" =: _mvePolicyDocument,
               toQuery
                 (toQueryList "item" <$> _mveRemoveRouteTableIds),
               "ResetPolicy" =: _mveResetPolicy,
               toQuery
                 (toQueryList "item" <$> _mveAddRouteTableIds),
               "DryRun" =: _mveDryRun,
               "VpcEndpointId" =: _mveVPCEndpointId]

-- | /See:/ 'modifyVPCEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mverReturn'
--
-- * 'mverStatus'
data ModifyVPCEndpointResponse = ModifyVPCEndpointResponse'
    { _mverReturn :: Maybe Bool
    , _mverStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'ModifyVPCEndpointResponse' smart constructor.
modifyVPCEndpointResponse :: Int -> ModifyVPCEndpointResponse
modifyVPCEndpointResponse pStatus =
    ModifyVPCEndpointResponse'
    { _mverReturn = Nothing
    , _mverStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mverReturn :: Lens' ModifyVPCEndpointResponse (Maybe Bool)
mverReturn = lens _mverReturn (\ s a -> s{_mverReturn = a});

-- | FIXME: Undocumented member.
mverStatus :: Lens' ModifyVPCEndpointResponse Int
mverStatus = lens _mverStatus (\ s a -> s{_mverStatus = a});
