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
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Internet gateway for use with a VPC. After creating the Internet gateway, you attach it to a VPC using 'AttachInternetGateway' .
--
--
-- For more information about your VPC and Internet gateway, see the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide> .
--
module Network.AWS.EC2.CreateInternetGateway
    (
    -- * Creating a Request
      createInternetGateway
    , CreateInternetGateway
    -- * Request Lenses
    , cigDryRun

    -- * Destructuring the Response
    , createInternetGatewayResponse
    , CreateInternetGatewayResponse
    -- * Response Lenses
    , cigrsInternetGateway
    , cigrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateInternetGateway.
--
--
--
-- /See:/ 'createInternetGateway' smart constructor.
newtype CreateInternetGateway = CreateInternetGateway'
  { _cigDryRun :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cigDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createInternetGateway
    :: CreateInternetGateway
createInternetGateway = CreateInternetGateway' {_cigDryRun = Nothing}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cigDryRun :: Lens' CreateInternetGateway (Maybe Bool)
cigDryRun = lens _cigDryRun (\ s a -> s{_cigDryRun = a})

instance AWSRequest CreateInternetGateway where
        type Rs CreateInternetGateway =
             CreateInternetGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateInternetGatewayResponse' <$>
                   (x .@? "internetGateway") <*> (pure (fromEnum s)))

instance Hashable CreateInternetGateway where

instance NFData CreateInternetGateway where

instance ToHeaders CreateInternetGateway where
        toHeaders = const mempty

instance ToPath CreateInternetGateway where
        toPath = const "/"

instance ToQuery CreateInternetGateway where
        toQuery CreateInternetGateway'{..}
          = mconcat
              ["Action" =: ("CreateInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cigDryRun]

-- | Contains the output of CreateInternetGateway.
--
--
--
-- /See:/ 'createInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { _cigrsInternetGateway :: !(Maybe InternetGateway)
  , _cigrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInternetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cigrsInternetGateway' - Information about the Internet gateway.
--
-- * 'cigrsResponseStatus' - -- | The response status code.
createInternetGatewayResponse
    :: Int -- ^ 'cigrsResponseStatus'
    -> CreateInternetGatewayResponse
createInternetGatewayResponse pResponseStatus_ =
  CreateInternetGatewayResponse'
    {_cigrsInternetGateway = Nothing, _cigrsResponseStatus = pResponseStatus_}


-- | Information about the Internet gateway.
cigrsInternetGateway :: Lens' CreateInternetGatewayResponse (Maybe InternetGateway)
cigrsInternetGateway = lens _cigrsInternetGateway (\ s a -> s{_cigrsInternetGateway = a})

-- | -- | The response status code.
cigrsResponseStatus :: Lens' CreateInternetGatewayResponse Int
cigrsResponseStatus = lens _cigrsResponseStatus (\ s a -> s{_cigrsResponseStatus = a})

instance NFData CreateInternetGatewayResponse where
