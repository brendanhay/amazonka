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
-- Module      : Network.AWS.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGatewayGroup
  ( -- * Creating a Request
    updateGatewayGroup,
    UpdateGatewayGroup,

    -- * Request Lenses
    uggName,
    uggDescription,
    uggGatewayGroupARN,

    -- * Destructuring the Response
    updateGatewayGroupResponse,
    UpdateGatewayGroupResponse,

    -- * Response Lenses
    uggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { _uggName ::
      !(Maybe Text),
    _uggDescription :: !(Maybe Text),
    _uggGatewayGroupARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGatewayGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uggName' - The updated name of the gateway group.
--
-- * 'uggDescription' - The updated description of the gateway group.
--
-- * 'uggGatewayGroupARN' - The ARN of the gateway group to update.
updateGatewayGroup ::
  -- | 'uggGatewayGroupARN'
  Text ->
  UpdateGatewayGroup
updateGatewayGroup pGatewayGroupARN_ =
  UpdateGatewayGroup'
    { _uggName = Nothing,
      _uggDescription = Nothing,
      _uggGatewayGroupARN = pGatewayGroupARN_
    }

-- | The updated name of the gateway group.
uggName :: Lens' UpdateGatewayGroup (Maybe Text)
uggName = lens _uggName (\s a -> s {_uggName = a})

-- | The updated description of the gateway group.
uggDescription :: Lens' UpdateGatewayGroup (Maybe Text)
uggDescription = lens _uggDescription (\s a -> s {_uggDescription = a})

-- | The ARN of the gateway group to update.
uggGatewayGroupARN :: Lens' UpdateGatewayGroup Text
uggGatewayGroupARN = lens _uggGatewayGroupARN (\s a -> s {_uggGatewayGroupARN = a})

instance AWSRequest UpdateGatewayGroup where
  type Rs UpdateGatewayGroup = UpdateGatewayGroupResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> UpdateGatewayGroupResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateGatewayGroup

instance NFData UpdateGatewayGroup

instance ToHeaders UpdateGatewayGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.UpdateGatewayGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateGatewayGroup where
  toJSON UpdateGatewayGroup' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _uggName,
            ("Description" .=) <$> _uggDescription,
            Just ("GatewayGroupArn" .= _uggGatewayGroupARN)
          ]
      )

instance ToPath UpdateGatewayGroup where
  toPath = const "/"

instance ToQuery UpdateGatewayGroup where
  toQuery = const mempty

-- | /See:/ 'updateGatewayGroupResponse' smart constructor.
newtype UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { _uggrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGatewayGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uggrsResponseStatus' - -- | The response status code.
updateGatewayGroupResponse ::
  -- | 'uggrsResponseStatus'
  Int ->
  UpdateGatewayGroupResponse
updateGatewayGroupResponse pResponseStatus_ =
  UpdateGatewayGroupResponse'
    { _uggrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uggrsResponseStatus :: Lens' UpdateGatewayGroupResponse Int
uggrsResponseStatus = lens _uggrsResponseStatus (\s a -> s {_uggrsResponseStatus = a})

instance NFData UpdateGatewayGroupResponse
