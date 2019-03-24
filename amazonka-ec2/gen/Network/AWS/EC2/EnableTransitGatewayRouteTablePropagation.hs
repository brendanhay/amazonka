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
-- Module      : Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified attachment to propagate routes to the specified propagation route table.
--
--
module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
    (
    -- * Creating a Request
      enableTransitGatewayRouteTablePropagation
    , EnableTransitGatewayRouteTablePropagation
    -- * Request Lenses
    , etgrtpDryRun
    , etgrtpTransitGatewayRouteTableId
    , etgrtpTransitGatewayAttachmentId

    -- * Destructuring the Response
    , enableTransitGatewayRouteTablePropagationResponse
    , EnableTransitGatewayRouteTablePropagationResponse
    -- * Response Lenses
    , etgrtprsPropagation
    , etgrtprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableTransitGatewayRouteTablePropagation' smart constructor.
data EnableTransitGatewayRouteTablePropagation = EnableTransitGatewayRouteTablePropagation'
  { _etgrtpDryRun                     :: !(Maybe Bool)
  , _etgrtpTransitGatewayRouteTableId :: !Text
  , _etgrtpTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableTransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etgrtpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'etgrtpTransitGatewayRouteTableId' - The ID of the propagation route table.
--
-- * 'etgrtpTransitGatewayAttachmentId' - The ID of the attachment.
enableTransitGatewayRouteTablePropagation
    :: Text -- ^ 'etgrtpTransitGatewayRouteTableId'
    -> Text -- ^ 'etgrtpTransitGatewayAttachmentId'
    -> EnableTransitGatewayRouteTablePropagation
enableTransitGatewayRouteTablePropagation pTransitGatewayRouteTableId_ pTransitGatewayAttachmentId_ =
  EnableTransitGatewayRouteTablePropagation'
    { _etgrtpDryRun = Nothing
    , _etgrtpTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    , _etgrtpTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
etgrtpDryRun :: Lens' EnableTransitGatewayRouteTablePropagation (Maybe Bool)
etgrtpDryRun = lens _etgrtpDryRun (\ s a -> s{_etgrtpDryRun = a})

-- | The ID of the propagation route table.
etgrtpTransitGatewayRouteTableId :: Lens' EnableTransitGatewayRouteTablePropagation Text
etgrtpTransitGatewayRouteTableId = lens _etgrtpTransitGatewayRouteTableId (\ s a -> s{_etgrtpTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
etgrtpTransitGatewayAttachmentId :: Lens' EnableTransitGatewayRouteTablePropagation Text
etgrtpTransitGatewayAttachmentId = lens _etgrtpTransitGatewayAttachmentId (\ s a -> s{_etgrtpTransitGatewayAttachmentId = a})

instance AWSRequest
           EnableTransitGatewayRouteTablePropagation
         where
        type Rs EnableTransitGatewayRouteTablePropagation =
             EnableTransitGatewayRouteTablePropagationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 EnableTransitGatewayRouteTablePropagationResponse'
                   <$> (x .@? "propagation") <*> (pure (fromEnum s)))

instance Hashable
           EnableTransitGatewayRouteTablePropagation
         where

instance NFData
           EnableTransitGatewayRouteTablePropagation
         where

instance ToHeaders
           EnableTransitGatewayRouteTablePropagation
         where
        toHeaders = const mempty

instance ToPath
           EnableTransitGatewayRouteTablePropagation
         where
        toPath = const "/"

instance ToQuery
           EnableTransitGatewayRouteTablePropagation
         where
        toQuery
          EnableTransitGatewayRouteTablePropagation'{..}
          = mconcat
              ["Action" =:
                 ("EnableTransitGatewayRouteTablePropagation" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _etgrtpDryRun,
               "TransitGatewayRouteTableId" =:
                 _etgrtpTransitGatewayRouteTableId,
               "TransitGatewayAttachmentId" =:
                 _etgrtpTransitGatewayAttachmentId]

-- | /See:/ 'enableTransitGatewayRouteTablePropagationResponse' smart constructor.
data EnableTransitGatewayRouteTablePropagationResponse = EnableTransitGatewayRouteTablePropagationResponse'
  { _etgrtprsPropagation    :: !(Maybe TransitGatewayPropagation)
  , _etgrtprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableTransitGatewayRouteTablePropagationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etgrtprsPropagation' - Information about route propagation.
--
-- * 'etgrtprsResponseStatus' - -- | The response status code.
enableTransitGatewayRouteTablePropagationResponse
    :: Int -- ^ 'etgrtprsResponseStatus'
    -> EnableTransitGatewayRouteTablePropagationResponse
enableTransitGatewayRouteTablePropagationResponse pResponseStatus_ =
  EnableTransitGatewayRouteTablePropagationResponse'
    {_etgrtprsPropagation = Nothing, _etgrtprsResponseStatus = pResponseStatus_}


-- | Information about route propagation.
etgrtprsPropagation :: Lens' EnableTransitGatewayRouteTablePropagationResponse (Maybe TransitGatewayPropagation)
etgrtprsPropagation = lens _etgrtprsPropagation (\ s a -> s{_etgrtprsPropagation = a})

-- | -- | The response status code.
etgrtprsResponseStatus :: Lens' EnableTransitGatewayRouteTablePropagationResponse Int
etgrtprsResponseStatus = lens _etgrtprsResponseStatus (\ s a -> s{_etgrtprsResponseStatus = a})

instance NFData
           EnableTransitGatewayRouteTablePropagationResponse
         where
