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
-- Module      : Network.AWS.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit gateway, the modified options are applied to new transit gateway attachments only. Your existing transit gateway attachments are not modified.
module Network.AWS.EC2.ModifyTransitGateway
  ( -- * Creating a Request
    modifyTransitGateway,
    ModifyTransitGateway,

    -- * Request Lenses
    mtgOptions,
    mtgDescription,
    mtgDryRun,
    mtgTransitGatewayId,

    -- * Destructuring the Response
    modifyTransitGatewayResponse,
    ModifyTransitGatewayResponse,

    -- * Response Lenses
    mtgrsTransitGateway,
    mtgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { _mtgOptions ::
      !(Maybe ModifyTransitGatewayOptions),
    _mtgDescription :: !(Maybe Text),
    _mtgDryRun :: !(Maybe Bool),
    _mtgTransitGatewayId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTransitGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgOptions' - The options to modify.
--
-- * 'mtgDescription' - The description for the transit gateway.
--
-- * 'mtgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mtgTransitGatewayId' - The ID of the transit gateway.
modifyTransitGateway ::
  -- | 'mtgTransitGatewayId'
  Text ->
  ModifyTransitGateway
modifyTransitGateway pTransitGatewayId_ =
  ModifyTransitGateway'
    { _mtgOptions = Nothing,
      _mtgDescription = Nothing,
      _mtgDryRun = Nothing,
      _mtgTransitGatewayId = pTransitGatewayId_
    }

-- | The options to modify.
mtgOptions :: Lens' ModifyTransitGateway (Maybe ModifyTransitGatewayOptions)
mtgOptions = lens _mtgOptions (\s a -> s {_mtgOptions = a})

-- | The description for the transit gateway.
mtgDescription :: Lens' ModifyTransitGateway (Maybe Text)
mtgDescription = lens _mtgDescription (\s a -> s {_mtgDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mtgDryRun :: Lens' ModifyTransitGateway (Maybe Bool)
mtgDryRun = lens _mtgDryRun (\s a -> s {_mtgDryRun = a})

-- | The ID of the transit gateway.
mtgTransitGatewayId :: Lens' ModifyTransitGateway Text
mtgTransitGatewayId = lens _mtgTransitGatewayId (\s a -> s {_mtgTransitGatewayId = a})

instance AWSRequest ModifyTransitGateway where
  type Rs ModifyTransitGateway = ModifyTransitGatewayResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyTransitGatewayResponse'
            <$> (x .@? "transitGateway") <*> (pure (fromEnum s))
      )

instance Hashable ModifyTransitGateway

instance NFData ModifyTransitGateway

instance ToHeaders ModifyTransitGateway where
  toHeaders = const mempty

instance ToPath ModifyTransitGateway where
  toPath = const "/"

instance ToQuery ModifyTransitGateway where
  toQuery ModifyTransitGateway' {..} =
    mconcat
      [ "Action" =: ("ModifyTransitGateway" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Options" =: _mtgOptions,
        "Description" =: _mtgDescription,
        "DryRun" =: _mtgDryRun,
        "TransitGatewayId" =: _mtgTransitGatewayId
      ]

-- | /See:/ 'modifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { _mtgrsTransitGateway ::
      !(Maybe TransitGateway),
    _mtgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyTransitGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgrsTransitGateway' - Undocumented member.
--
-- * 'mtgrsResponseStatus' - -- | The response status code.
modifyTransitGatewayResponse ::
  -- | 'mtgrsResponseStatus'
  Int ->
  ModifyTransitGatewayResponse
modifyTransitGatewayResponse pResponseStatus_ =
  ModifyTransitGatewayResponse'
    { _mtgrsTransitGateway = Nothing,
      _mtgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mtgrsTransitGateway :: Lens' ModifyTransitGatewayResponse (Maybe TransitGateway)
mtgrsTransitGateway = lens _mtgrsTransitGateway (\s a -> s {_mtgrsTransitGateway = a})

-- | -- | The response status code.
mtgrsResponseStatus :: Lens' ModifyTransitGatewayResponse Int
mtgrsResponseStatus = lens _mtgrsResponseStatus (\s a -> s {_mtgrsResponseStatus = a})

instance NFData ModifyTransitGatewayResponse
