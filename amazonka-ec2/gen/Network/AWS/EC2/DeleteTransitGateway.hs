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
-- Module      : Network.AWS.EC2.DeleteTransitGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway.
--
--
module Network.AWS.EC2.DeleteTransitGateway
    (
    -- * Creating a Request
      deleteTransitGateway
    , DeleteTransitGateway
    -- * Request Lenses
    , dtgDryRun
    , dtgTransitGatewayId

    -- * Destructuring the Response
    , deleteTransitGatewayResponse
    , DeleteTransitGatewayResponse
    -- * Response Lenses
    , dtgtrsTransitGateway
    , dtgtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGateway' smart constructor.
data DeleteTransitGateway = DeleteTransitGateway'
  { _dtgDryRun           :: !(Maybe Bool)
  , _dtgTransitGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgTransitGatewayId' - The ID of the transit gateway.
deleteTransitGateway
    :: Text -- ^ 'dtgTransitGatewayId'
    -> DeleteTransitGateway
deleteTransitGateway pTransitGatewayId_ =
  DeleteTransitGateway'
    {_dtgDryRun = Nothing, _dtgTransitGatewayId = pTransitGatewayId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgDryRun :: Lens' DeleteTransitGateway (Maybe Bool)
dtgDryRun = lens _dtgDryRun (\ s a -> s{_dtgDryRun = a})

-- | The ID of the transit gateway.
dtgTransitGatewayId :: Lens' DeleteTransitGateway Text
dtgTransitGatewayId = lens _dtgTransitGatewayId (\ s a -> s{_dtgTransitGatewayId = a})

instance AWSRequest DeleteTransitGateway where
        type Rs DeleteTransitGateway =
             DeleteTransitGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayResponse' <$>
                   (x .@? "transitGateway") <*> (pure (fromEnum s)))

instance Hashable DeleteTransitGateway where

instance NFData DeleteTransitGateway where

instance ToHeaders DeleteTransitGateway where
        toHeaders = const mempty

instance ToPath DeleteTransitGateway where
        toPath = const "/"

instance ToQuery DeleteTransitGateway where
        toQuery DeleteTransitGateway'{..}
          = mconcat
              ["Action" =: ("DeleteTransitGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgDryRun,
               "TransitGatewayId" =: _dtgTransitGatewayId]

-- | /See:/ 'deleteTransitGatewayResponse' smart constructor.
data DeleteTransitGatewayResponse = DeleteTransitGatewayResponse'
  { _dtgtrsTransitGateway :: !(Maybe TransitGateway)
  , _dtgtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTransitGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgtrsTransitGateway' - Information about the deleted transit gateway.
--
-- * 'dtgtrsResponseStatus' - -- | The response status code.
deleteTransitGatewayResponse
    :: Int -- ^ 'dtgtrsResponseStatus'
    -> DeleteTransitGatewayResponse
deleteTransitGatewayResponse pResponseStatus_ =
  DeleteTransitGatewayResponse'
    {_dtgtrsTransitGateway = Nothing, _dtgtrsResponseStatus = pResponseStatus_}


-- | Information about the deleted transit gateway.
dtgtrsTransitGateway :: Lens' DeleteTransitGatewayResponse (Maybe TransitGateway)
dtgtrsTransitGateway = lens _dtgtrsTransitGateway (\ s a -> s{_dtgtrsTransitGateway = a})

-- | -- | The response status code.
dtgtrsResponseStatus :: Lens' DeleteTransitGatewayResponse Int
dtgtrsResponseStatus = lens _dtgtrsResponseStatus (\ s a -> s{_dtgtrsResponseStatus = a})

instance NFData DeleteTransitGatewayResponse where
