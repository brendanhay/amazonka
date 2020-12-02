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
-- Module      : Network.AWS.Pinpoint.RemoveAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more attributes, of the same attribute type, from all the endpoints that are associated with an application.
module Network.AWS.Pinpoint.RemoveAttributes
  ( -- * Creating a Request
    removeAttributes,
    RemoveAttributes,

    -- * Request Lenses
    raAttributeType,
    raApplicationId,
    raUpdateAttributesRequest,

    -- * Destructuring the Response
    removeAttributesResponse,
    RemoveAttributesResponse,

    -- * Response Lenses
    rarsResponseStatus,
    rarsAttributesResource,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeAttributes' smart constructor.
data RemoveAttributes = RemoveAttributes'
  { _raAttributeType ::
      !Text,
    _raApplicationId :: !Text,
    _raUpdateAttributesRequest :: !UpdateAttributesRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAttributeType' - The type of attribute or attributes to remove. Valid values are:     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
--
-- * 'raApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'raUpdateAttributesRequest' - Undocumented member.
removeAttributes ::
  -- | 'raAttributeType'
  Text ->
  -- | 'raApplicationId'
  Text ->
  -- | 'raUpdateAttributesRequest'
  UpdateAttributesRequest ->
  RemoveAttributes
removeAttributes
  pAttributeType_
  pApplicationId_
  pUpdateAttributesRequest_ =
    RemoveAttributes'
      { _raAttributeType = pAttributeType_,
        _raApplicationId = pApplicationId_,
        _raUpdateAttributesRequest = pUpdateAttributesRequest_
      }

-- | The type of attribute or attributes to remove. Valid values are:     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
raAttributeType :: Lens' RemoveAttributes Text
raAttributeType = lens _raAttributeType (\s a -> s {_raAttributeType = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
raApplicationId :: Lens' RemoveAttributes Text
raApplicationId = lens _raApplicationId (\s a -> s {_raApplicationId = a})

-- | Undocumented member.
raUpdateAttributesRequest :: Lens' RemoveAttributes UpdateAttributesRequest
raUpdateAttributesRequest = lens _raUpdateAttributesRequest (\s a -> s {_raUpdateAttributesRequest = a})

instance AWSRequest RemoveAttributes where
  type Rs RemoveAttributes = RemoveAttributesResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          RemoveAttributesResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable RemoveAttributes

instance NFData RemoveAttributes

instance ToHeaders RemoveAttributes where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON RemoveAttributes where
  toJSON RemoveAttributes' {..} =
    object
      ( catMaybes
          [Just ("UpdateAttributesRequest" .= _raUpdateAttributesRequest)]
      )

instance ToPath RemoveAttributes where
  toPath RemoveAttributes' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _raApplicationId,
        "/attributes/",
        toBS _raAttributeType
      ]

instance ToQuery RemoveAttributes where
  toQuery = const mempty

-- | /See:/ 'removeAttributesResponse' smart constructor.
data RemoveAttributesResponse = RemoveAttributesResponse'
  { _rarsResponseStatus ::
      !Int,
    _rarsAttributesResource ::
      !AttributesResource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsResponseStatus' - -- | The response status code.
--
-- * 'rarsAttributesResource' - Undocumented member.
removeAttributesResponse ::
  -- | 'rarsResponseStatus'
  Int ->
  -- | 'rarsAttributesResource'
  AttributesResource ->
  RemoveAttributesResponse
removeAttributesResponse pResponseStatus_ pAttributesResource_ =
  RemoveAttributesResponse'
    { _rarsResponseStatus = pResponseStatus_,
      _rarsAttributesResource = pAttributesResource_
    }

-- | -- | The response status code.
rarsResponseStatus :: Lens' RemoveAttributesResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\s a -> s {_rarsResponseStatus = a})

-- | Undocumented member.
rarsAttributesResource :: Lens' RemoveAttributesResponse AttributesResource
rarsAttributesResource = lens _rarsAttributesResource (\s a -> s {_rarsAttributesResource = a})

instance NFData RemoveAttributesResponse
