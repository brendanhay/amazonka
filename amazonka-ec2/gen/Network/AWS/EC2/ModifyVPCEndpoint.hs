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
-- Module      : Network.AWS.EC2.ModifyVPCEndpoint
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. You can modify the policy associated with the endpoint, and you can add and remove route tables associated with the endpoint.
--
--
module Network.AWS.EC2.ModifyVPCEndpoint
    (
    -- * Creating a Request
      modifyVPCEndpoint
    , ModifyVPCEndpoint
    -- * Request Lenses
    , mvePolicyDocument
    , mveRemoveRouteTableIds
    , mveResetPolicy
    , mveAddRouteTableIds
    , mveDryRun
    , mveVPCEndpointId

    -- * Destructuring the Response
    , modifyVPCEndpointResponse
    , ModifyVPCEndpointResponse
    -- * Response Lenses
    , mversReturn
    , mversResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
--
--
-- /See:/ 'modifyVPCEndpoint' smart constructor.
data ModifyVPCEndpoint = ModifyVPCEndpoint'
  { _mvePolicyDocument      :: {-# NOUNPACK #-}!(Maybe Text)
  , _mveRemoveRouteTableIds :: {-# NOUNPACK #-}!(Maybe [Text])
  , _mveResetPolicy         :: {-# NOUNPACK #-}!(Maybe Bool)
  , _mveAddRouteTableIds    :: {-# NOUNPACK #-}!(Maybe [Text])
  , _mveDryRun              :: {-# NOUNPACK #-}!(Maybe Bool)
  , _mveVPCEndpointId       :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvePolicyDocument' - A policy document to attach to the endpoint. The policy must be in valid JSON format.
--
-- * 'mveRemoveRouteTableIds' - One or more route table IDs to disassociate from the endpoint.
--
-- * 'mveResetPolicy' - Specify @true@ to reset the policy document to the default policy. The default policy allows access to the service.
--
-- * 'mveAddRouteTableIds' - One or more route tables IDs to associate with the endpoint.
--
-- * 'mveDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mveVPCEndpointId' - The ID of the endpoint.
modifyVPCEndpoint
    :: Text -- ^ 'mveVPCEndpointId'
    -> ModifyVPCEndpoint
modifyVPCEndpoint pVPCEndpointId_ =
  ModifyVPCEndpoint'
  { _mvePolicyDocument = Nothing
  , _mveRemoveRouteTableIds = Nothing
  , _mveResetPolicy = Nothing
  , _mveAddRouteTableIds = Nothing
  , _mveDryRun = Nothing
  , _mveVPCEndpointId = pVPCEndpointId_
  }


-- | A policy document to attach to the endpoint. The policy must be in valid JSON format.
mvePolicyDocument :: Lens' ModifyVPCEndpoint (Maybe Text)
mvePolicyDocument = lens _mvePolicyDocument (\ s a -> s{_mvePolicyDocument = a});

-- | One or more route table IDs to disassociate from the endpoint.
mveRemoveRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveRemoveRouteTableIds = lens _mveRemoveRouteTableIds (\ s a -> s{_mveRemoveRouteTableIds = a}) . _Default . _Coerce;

-- | Specify @true@ to reset the policy document to the default policy. The default policy allows access to the service.
mveResetPolicy :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveResetPolicy = lens _mveResetPolicy (\ s a -> s{_mveResetPolicy = a});

-- | One or more route tables IDs to associate with the endpoint.
mveAddRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveAddRouteTableIds = lens _mveAddRouteTableIds (\ s a -> s{_mveAddRouteTableIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mveDryRun :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveDryRun = lens _mveDryRun (\ s a -> s{_mveDryRun = a});

-- | The ID of the endpoint.
mveVPCEndpointId :: Lens' ModifyVPCEndpoint Text
mveVPCEndpointId = lens _mveVPCEndpointId (\ s a -> s{_mveVPCEndpointId = a});

instance AWSRequest ModifyVPCEndpoint where
        type Rs ModifyVPCEndpoint = ModifyVPCEndpointResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCEndpointResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyVPCEndpoint where

instance NFData ModifyVPCEndpoint where

instance ToHeaders ModifyVPCEndpoint where
        toHeaders = const mempty

instance ToPath ModifyVPCEndpoint where
        toPath = const "/"

instance ToQuery ModifyVPCEndpoint where
        toQuery ModifyVPCEndpoint'{..}
          = mconcat
              ["Action" =: ("ModifyVpcEndpoint" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PolicyDocument" =: _mvePolicyDocument,
               toQuery
                 (toQueryList "RemoveRouteTableId" <$>
                    _mveRemoveRouteTableIds),
               "ResetPolicy" =: _mveResetPolicy,
               toQuery
                 (toQueryList "AddRouteTableId" <$>
                    _mveAddRouteTableIds),
               "DryRun" =: _mveDryRun,
               "VpcEndpointId" =: _mveVPCEndpointId]

-- | Contains the output of ModifyVpcEndpoint.
--
--
--
-- /See:/ 'modifyVPCEndpointResponse' smart constructor.
data ModifyVPCEndpointResponse = ModifyVPCEndpointResponse'
  { _mversReturn         :: {-# NOUNPACK #-}!(Maybe Bool)
  , _mversResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mversReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'mversResponseStatus' - -- | The response status code.
modifyVPCEndpointResponse
    :: Int -- ^ 'mversResponseStatus'
    -> ModifyVPCEndpointResponse
modifyVPCEndpointResponse pResponseStatus_ =
  ModifyVPCEndpointResponse'
  {_mversReturn = Nothing, _mversResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mversReturn :: Lens' ModifyVPCEndpointResponse (Maybe Bool)
mversReturn = lens _mversReturn (\ s a -> s{_mversReturn = a});

-- | -- | The response status code.
mversResponseStatus :: Lens' ModifyVPCEndpointResponse Int
mversResponseStatus = lens _mversResponseStatus (\ s a -> s{_mversResponseStatus = a});

instance NFData ModifyVPCEndpointResponse where
