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
-- Module      : Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified resource attachment from propagating routes to the specified propagation route table.
--
--
module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
    (
    -- * Creating a Request
      disableTransitGatewayRouteTablePropagation
    , DisableTransitGatewayRouteTablePropagation
    -- * Request Lenses
    , dtgrtpDryRun
    , dtgrtpTransitGatewayRouteTableId
    , dtgrtpTransitGatewayAttachmentId

    -- * Destructuring the Response
    , disableTransitGatewayRouteTablePropagationResponse
    , DisableTransitGatewayRouteTablePropagationResponse
    -- * Response Lenses
    , dtgrtprsPropagation
    , dtgrtprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableTransitGatewayRouteTablePropagation' smart constructor.
data DisableTransitGatewayRouteTablePropagation = DisableTransitGatewayRouteTablePropagation'
  { _dtgrtpDryRun                     :: !(Maybe Bool)
  , _dtgrtpTransitGatewayRouteTableId :: !Text
  , _dtgrtpTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableTransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrtpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgrtpTransitGatewayRouteTableId' - The ID of the propagation route table.
--
-- * 'dtgrtpTransitGatewayAttachmentId' - The ID of the attachment.
disableTransitGatewayRouteTablePropagation
    :: Text -- ^ 'dtgrtpTransitGatewayRouteTableId'
    -> Text -- ^ 'dtgrtpTransitGatewayAttachmentId'
    -> DisableTransitGatewayRouteTablePropagation
disableTransitGatewayRouteTablePropagation pTransitGatewayRouteTableId_ pTransitGatewayAttachmentId_ =
  DisableTransitGatewayRouteTablePropagation'
    { _dtgrtpDryRun = Nothing
    , _dtgrtpTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    , _dtgrtpTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgrtpDryRun :: Lens' DisableTransitGatewayRouteTablePropagation (Maybe Bool)
dtgrtpDryRun = lens _dtgrtpDryRun (\ s a -> s{_dtgrtpDryRun = a})

-- | The ID of the propagation route table.
dtgrtpTransitGatewayRouteTableId :: Lens' DisableTransitGatewayRouteTablePropagation Text
dtgrtpTransitGatewayRouteTableId = lens _dtgrtpTransitGatewayRouteTableId (\ s a -> s{_dtgrtpTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
dtgrtpTransitGatewayAttachmentId :: Lens' DisableTransitGatewayRouteTablePropagation Text
dtgrtpTransitGatewayAttachmentId = lens _dtgrtpTransitGatewayAttachmentId (\ s a -> s{_dtgrtpTransitGatewayAttachmentId = a})

instance AWSRequest
           DisableTransitGatewayRouteTablePropagation
         where
        type Rs DisableTransitGatewayRouteTablePropagation =
             DisableTransitGatewayRouteTablePropagationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisableTransitGatewayRouteTablePropagationResponse'
                   <$> (x .@? "propagation") <*> (pure (fromEnum s)))

instance Hashable
           DisableTransitGatewayRouteTablePropagation
         where

instance NFData
           DisableTransitGatewayRouteTablePropagation
         where

instance ToHeaders
           DisableTransitGatewayRouteTablePropagation
         where
        toHeaders = const mempty

instance ToPath
           DisableTransitGatewayRouteTablePropagation
         where
        toPath = const "/"

instance ToQuery
           DisableTransitGatewayRouteTablePropagation
         where
        toQuery
          DisableTransitGatewayRouteTablePropagation'{..}
          = mconcat
              ["Action" =:
                 ("DisableTransitGatewayRouteTablePropagation" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgrtpDryRun,
               "TransitGatewayRouteTableId" =:
                 _dtgrtpTransitGatewayRouteTableId,
               "TransitGatewayAttachmentId" =:
                 _dtgrtpTransitGatewayAttachmentId]

-- | /See:/ 'disableTransitGatewayRouteTablePropagationResponse' smart constructor.
data DisableTransitGatewayRouteTablePropagationResponse = DisableTransitGatewayRouteTablePropagationResponse'
  { _dtgrtprsPropagation    :: !(Maybe TransitGatewayPropagation)
  , _dtgrtprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableTransitGatewayRouteTablePropagationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrtprsPropagation' - Information about route propagation.
--
-- * 'dtgrtprsResponseStatus' - -- | The response status code.
disableTransitGatewayRouteTablePropagationResponse
    :: Int -- ^ 'dtgrtprsResponseStatus'
    -> DisableTransitGatewayRouteTablePropagationResponse
disableTransitGatewayRouteTablePropagationResponse pResponseStatus_ =
  DisableTransitGatewayRouteTablePropagationResponse'
    {_dtgrtprsPropagation = Nothing, _dtgrtprsResponseStatus = pResponseStatus_}


-- | Information about route propagation.
dtgrtprsPropagation :: Lens' DisableTransitGatewayRouteTablePropagationResponse (Maybe TransitGatewayPropagation)
dtgrtprsPropagation = lens _dtgrtprsPropagation (\ s a -> s{_dtgrtprsPropagation = a})

-- | -- | The response status code.
dtgrtprsResponseStatus :: Lens' DisableTransitGatewayRouteTablePropagationResponse Int
dtgrtprsResponseStatus = lens _dtgrtprsResponseStatus (\ s a -> s{_dtgrtprsResponseStatus = a})

instance NFData
           DisableTransitGatewayRouteTablePropagationResponse
         where
