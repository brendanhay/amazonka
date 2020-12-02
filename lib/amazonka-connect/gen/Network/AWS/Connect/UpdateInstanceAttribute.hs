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
-- Module      : Network.AWS.Connect.UpdateInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the value for the specified attribute type.
module Network.AWS.Connect.UpdateInstanceAttribute
  ( -- * Creating a Request
    updateInstanceAttribute,
    UpdateInstanceAttribute,

    -- * Request Lenses
    uiaInstanceId,
    uiaAttributeType,
    uiaValue,

    -- * Destructuring the Response
    updateInstanceAttributeResponse,
    UpdateInstanceAttributeResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateInstanceAttribute' smart constructor.
data UpdateInstanceAttribute = UpdateInstanceAttribute'
  { _uiaInstanceId ::
      !Text,
    _uiaAttributeType :: !InstanceAttributeType,
    _uiaValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiaInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'uiaAttributeType' - The type of attribute.
--
-- * 'uiaValue' - The value for the attribute. Maximum character limit is 100.
updateInstanceAttribute ::
  -- | 'uiaInstanceId'
  Text ->
  -- | 'uiaAttributeType'
  InstanceAttributeType ->
  -- | 'uiaValue'
  Text ->
  UpdateInstanceAttribute
updateInstanceAttribute pInstanceId_ pAttributeType_ pValue_ =
  UpdateInstanceAttribute'
    { _uiaInstanceId = pInstanceId_,
      _uiaAttributeType = pAttributeType_,
      _uiaValue = pValue_
    }

-- | The identifier of the Amazon Connect instance.
uiaInstanceId :: Lens' UpdateInstanceAttribute Text
uiaInstanceId = lens _uiaInstanceId (\s a -> s {_uiaInstanceId = a})

-- | The type of attribute.
uiaAttributeType :: Lens' UpdateInstanceAttribute InstanceAttributeType
uiaAttributeType = lens _uiaAttributeType (\s a -> s {_uiaAttributeType = a})

-- | The value for the attribute. Maximum character limit is 100.
uiaValue :: Lens' UpdateInstanceAttribute Text
uiaValue = lens _uiaValue (\s a -> s {_uiaValue = a})

instance AWSRequest UpdateInstanceAttribute where
  type Rs UpdateInstanceAttribute = UpdateInstanceAttributeResponse
  request = postJSON connect
  response = receiveNull UpdateInstanceAttributeResponse'

instance Hashable UpdateInstanceAttribute

instance NFData UpdateInstanceAttribute

instance ToHeaders UpdateInstanceAttribute where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateInstanceAttribute where
  toJSON UpdateInstanceAttribute' {..} =
    object (catMaybes [Just ("Value" .= _uiaValue)])

instance ToPath UpdateInstanceAttribute where
  toPath UpdateInstanceAttribute' {..} =
    mconcat
      [ "/instance/",
        toBS _uiaInstanceId,
        "/attribute/",
        toBS _uiaAttributeType
      ]

instance ToQuery UpdateInstanceAttribute where
  toQuery = const mempty

-- | /See:/ 'updateInstanceAttributeResponse' smart constructor.
data UpdateInstanceAttributeResponse = UpdateInstanceAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInstanceAttributeResponse' with the minimum fields required to make a request.
updateInstanceAttributeResponse ::
  UpdateInstanceAttributeResponse
updateInstanceAttributeResponse = UpdateInstanceAttributeResponse'

instance NFData UpdateInstanceAttributeResponse
