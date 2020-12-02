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
-- Module      : Network.AWS.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Network.AWS.AlexaBusiness.GetGatewayGroup
  ( -- * Creating a Request
    getGatewayGroup,
    GetGatewayGroup,

    -- * Request Lenses
    gggGatewayGroupARN,

    -- * Destructuring the Response
    getGatewayGroupResponse,
    GetGatewayGroupResponse,

    -- * Response Lenses
    gggrsGatewayGroup,
    gggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGatewayGroup' smart constructor.
newtype GetGatewayGroup = GetGatewayGroup'
  { _gggGatewayGroupARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGatewayGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gggGatewayGroupARN' - The ARN of the gateway group to get.
getGatewayGroup ::
  -- | 'gggGatewayGroupARN'
  Text ->
  GetGatewayGroup
getGatewayGroup pGatewayGroupARN_ =
  GetGatewayGroup' {_gggGatewayGroupARN = pGatewayGroupARN_}

-- | The ARN of the gateway group to get.
gggGatewayGroupARN :: Lens' GetGatewayGroup Text
gggGatewayGroupARN = lens _gggGatewayGroupARN (\s a -> s {_gggGatewayGroupARN = a})

instance AWSRequest GetGatewayGroup where
  type Rs GetGatewayGroup = GetGatewayGroupResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          GetGatewayGroupResponse'
            <$> (x .?> "GatewayGroup") <*> (pure (fromEnum s))
      )

instance Hashable GetGatewayGroup

instance NFData GetGatewayGroup

instance ToHeaders GetGatewayGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.GetGatewayGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetGatewayGroup where
  toJSON GetGatewayGroup' {..} =
    object
      (catMaybes [Just ("GatewayGroupArn" .= _gggGatewayGroupARN)])

instance ToPath GetGatewayGroup where
  toPath = const "/"

instance ToQuery GetGatewayGroup where
  toQuery = const mempty

-- | /See:/ 'getGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { _gggrsGatewayGroup ::
      !(Maybe GatewayGroup),
    _gggrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGatewayGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gggrsGatewayGroup' - Undocumented member.
--
-- * 'gggrsResponseStatus' - -- | The response status code.
getGatewayGroupResponse ::
  -- | 'gggrsResponseStatus'
  Int ->
  GetGatewayGroupResponse
getGatewayGroupResponse pResponseStatus_ =
  GetGatewayGroupResponse'
    { _gggrsGatewayGroup = Nothing,
      _gggrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gggrsGatewayGroup :: Lens' GetGatewayGroupResponse (Maybe GatewayGroup)
gggrsGatewayGroup = lens _gggrsGatewayGroup (\s a -> s {_gggrsGatewayGroup = a})

-- | -- | The response status code.
gggrsResponseStatus :: Lens' GetGatewayGroupResponse Int
gggrsResponseStatus = lens _gggrsResponseStatus (\s a -> s {_gggrsResponseStatus = a})

instance NFData GetGatewayGroupResponse
