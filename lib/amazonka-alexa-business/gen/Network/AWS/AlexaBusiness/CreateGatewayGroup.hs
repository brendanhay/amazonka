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
-- Module      : Network.AWS.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Network.AWS.AlexaBusiness.CreateGatewayGroup
  ( -- * Creating a Request
    createGatewayGroup,
    CreateGatewayGroup,

    -- * Request Lenses
    cggDescription,
    cggName,
    cggClientRequestToken,

    -- * Destructuring the Response
    createGatewayGroupResponse,
    CreateGatewayGroupResponse,

    -- * Response Lenses
    cggrsGatewayGroupARN,
    cggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { _cggDescription ::
      !(Maybe Text),
    _cggName :: !Text,
    _cggClientRequestToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGatewayGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cggDescription' - The description of the gateway group.
--
-- * 'cggName' - The name of the gateway group.
--
-- * 'cggClientRequestToken' - A unique, user-specified identifier for the request that ensures idempotency.
createGatewayGroup ::
  -- | 'cggName'
  Text ->
  -- | 'cggClientRequestToken'
  Text ->
  CreateGatewayGroup
createGatewayGroup pName_ pClientRequestToken_ =
  CreateGatewayGroup'
    { _cggDescription = Nothing,
      _cggName = pName_,
      _cggClientRequestToken = pClientRequestToken_
    }

-- | The description of the gateway group.
cggDescription :: Lens' CreateGatewayGroup (Maybe Text)
cggDescription = lens _cggDescription (\s a -> s {_cggDescription = a})

-- | The name of the gateway group.
cggName :: Lens' CreateGatewayGroup Text
cggName = lens _cggName (\s a -> s {_cggName = a})

-- | A unique, user-specified identifier for the request that ensures idempotency.
cggClientRequestToken :: Lens' CreateGatewayGroup Text
cggClientRequestToken = lens _cggClientRequestToken (\s a -> s {_cggClientRequestToken = a})

instance AWSRequest CreateGatewayGroup where
  type Rs CreateGatewayGroup = CreateGatewayGroupResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          CreateGatewayGroupResponse'
            <$> (x .?> "GatewayGroupArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateGatewayGroup

instance NFData CreateGatewayGroup

instance ToHeaders CreateGatewayGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.CreateGatewayGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateGatewayGroup where
  toJSON CreateGatewayGroup' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _cggDescription,
            Just ("Name" .= _cggName),
            Just ("ClientRequestToken" .= _cggClientRequestToken)
          ]
      )

instance ToPath CreateGatewayGroup where
  toPath = const "/"

instance ToQuery CreateGatewayGroup where
  toQuery = const mempty

-- | /See:/ 'createGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { _cggrsGatewayGroupARN ::
      !(Maybe Text),
    _cggrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGatewayGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cggrsGatewayGroupARN' - The ARN of the created gateway group.
--
-- * 'cggrsResponseStatus' - -- | The response status code.
createGatewayGroupResponse ::
  -- | 'cggrsResponseStatus'
  Int ->
  CreateGatewayGroupResponse
createGatewayGroupResponse pResponseStatus_ =
  CreateGatewayGroupResponse'
    { _cggrsGatewayGroupARN = Nothing,
      _cggrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the created gateway group.
cggrsGatewayGroupARN :: Lens' CreateGatewayGroupResponse (Maybe Text)
cggrsGatewayGroupARN = lens _cggrsGatewayGroupARN (\s a -> s {_cggrsGatewayGroupARN = a})

-- | -- | The response status code.
cggrsResponseStatus :: Lens' CreateGatewayGroupResponse Int
cggrsResponseStatus = lens _cggrsResponseStatus (\s a -> s {_cggrsResponseStatus = a})

instance NFData CreateGatewayGroupResponse
