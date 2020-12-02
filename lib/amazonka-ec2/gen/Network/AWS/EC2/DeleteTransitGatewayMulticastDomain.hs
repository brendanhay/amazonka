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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
  ( -- * Creating a Request
    deleteTransitGatewayMulticastDomain,
    DeleteTransitGatewayMulticastDomain,

    -- * Request Lenses
    dtgmdDryRun,
    dtgmdTransitGatewayMulticastDomainId,

    -- * Destructuring the Response
    deleteTransitGatewayMulticastDomainResponse,
    DeleteTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    delrsTransitGatewayMulticastDomain,
    delrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { _dtgmdDryRun ::
      !(Maybe Bool),
    _dtgmdTransitGatewayMulticastDomainId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgmdTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
deleteTransitGatewayMulticastDomain ::
  -- | 'dtgmdTransitGatewayMulticastDomainId'
  Text ->
  DeleteTransitGatewayMulticastDomain
deleteTransitGatewayMulticastDomain
  pTransitGatewayMulticastDomainId_ =
    DeleteTransitGatewayMulticastDomain'
      { _dtgmdDryRun = Nothing,
        _dtgmdTransitGatewayMulticastDomainId =
          pTransitGatewayMulticastDomainId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgmdDryRun :: Lens' DeleteTransitGatewayMulticastDomain (Maybe Bool)
dtgmdDryRun = lens _dtgmdDryRun (\s a -> s {_dtgmdDryRun = a})

-- | The ID of the transit gateway multicast domain.
dtgmdTransitGatewayMulticastDomainId :: Lens' DeleteTransitGatewayMulticastDomain Text
dtgmdTransitGatewayMulticastDomainId = lens _dtgmdTransitGatewayMulticastDomainId (\s a -> s {_dtgmdTransitGatewayMulticastDomainId = a})

instance AWSRequest DeleteTransitGatewayMulticastDomain where
  type
    Rs DeleteTransitGatewayMulticastDomain =
      DeleteTransitGatewayMulticastDomainResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteTransitGatewayMulticastDomainResponse'
            <$> (x .@? "transitGatewayMulticastDomain") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTransitGatewayMulticastDomain

instance NFData DeleteTransitGatewayMulticastDomain

instance ToHeaders DeleteTransitGatewayMulticastDomain where
  toHeaders = const mempty

instance ToPath DeleteTransitGatewayMulticastDomain where
  toPath = const "/"

instance ToQuery DeleteTransitGatewayMulticastDomain where
  toQuery DeleteTransitGatewayMulticastDomain' {..} =
    mconcat
      [ "Action" =: ("DeleteTransitGatewayMulticastDomain" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dtgmdDryRun,
        "TransitGatewayMulticastDomainId"
          =: _dtgmdTransitGatewayMulticastDomainId
      ]

-- | /See:/ 'deleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { _delrsTransitGatewayMulticastDomain ::
      !( Maybe
           TransitGatewayMulticastDomain
       ),
    _delrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsTransitGatewayMulticastDomain' - Information about the deleted transit gateway multicast domain.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteTransitGatewayMulticastDomainResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteTransitGatewayMulticastDomainResponse
deleteTransitGatewayMulticastDomainResponse pResponseStatus_ =
  DeleteTransitGatewayMulticastDomainResponse'
    { _delrsTransitGatewayMulticastDomain =
        Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | Information about the deleted transit gateway multicast domain.
delrsTransitGatewayMulticastDomain :: Lens' DeleteTransitGatewayMulticastDomainResponse (Maybe TransitGatewayMulticastDomain)
delrsTransitGatewayMulticastDomain = lens _delrsTransitGatewayMulticastDomain (\s a -> s {_delrsTransitGatewayMulticastDomain = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteTransitGatewayMulticastDomainResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteTransitGatewayMulticastDomainResponse
