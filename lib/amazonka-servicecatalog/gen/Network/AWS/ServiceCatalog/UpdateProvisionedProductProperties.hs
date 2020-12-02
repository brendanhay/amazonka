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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the properties of the specified provisioned product.
module Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
  ( -- * Creating a Request
    updateProvisionedProductProperties,
    UpdateProvisionedProductProperties,

    -- * Request Lenses
    upppAcceptLanguage,
    upppProvisionedProductId,
    upppProvisionedProductProperties,
    upppIdempotencyToken,

    -- * Destructuring the Response
    updateProvisionedProductPropertiesResponse,
    UpdateProvisionedProductPropertiesResponse,

    -- * Response Lenses
    uppprsStatus,
    uppprsProvisionedProductProperties,
    uppprsRecordId,
    uppprsProvisionedProductId,
    uppprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'updateProvisionedProductProperties' smart constructor.
data UpdateProvisionedProductProperties = UpdateProvisionedProductProperties'
  { _upppAcceptLanguage ::
      !(Maybe Text),
    _upppProvisionedProductId ::
      !Text,
    _upppProvisionedProductProperties ::
      !( Map
           PropertyKey
           (Text)
       ),
    _upppIdempotencyToken ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisionedProductProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'upppProvisionedProductId' - The identifier of the provisioned product.
--
-- * 'upppProvisionedProductProperties' - A map that contains the provisioned product properties to be updated. The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an administrator to call @UpdateProvisionedProductProperties@ to update the launch role that is associated with a provisioned product. This role is used when an end user calls a provisioning operation such as @UpdateProvisionedProduct@ , @TerminateProvisionedProduct@ , or @ExecuteProvisionedProductServiceAction@ . Only a role ARN is valid. A user ARN is invalid.  The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user that has permission to see, update, terminate, and execute service actions in the provisioned product. The administrator can change the owner of a provisioned product to another IAM user within the same account. Both end user owners and administrators can see ownership history of the provisioned product using the @ListRecordHistory@ API. The new owner can describe all past records for the provisioned product using the @DescribeRecord@ API. The previous owner can no longer use @DescribeRecord@ , but can still see the product's history from when he was an owner using @ListRecordHistory@ . If a provisioned product ownership is assigned to an end user, they can see and perform any action through the API or Service Catalog console such as update, terminate, and execute service actions. If an end user provisions a product and the owner is updated to someone else, they will no longer be able to see or perform any actions through API or the Service Catalog console on that provisioned product.
--
-- * 'upppIdempotencyToken' - The idempotency token that uniquely identifies the provisioning product update request.
updateProvisionedProductProperties ::
  -- | 'upppProvisionedProductId'
  Text ->
  -- | 'upppIdempotencyToken'
  Text ->
  UpdateProvisionedProductProperties
updateProvisionedProductProperties
  pProvisionedProductId_
  pIdempotencyToken_ =
    UpdateProvisionedProductProperties'
      { _upppAcceptLanguage =
          Nothing,
        _upppProvisionedProductId = pProvisionedProductId_,
        _upppProvisionedProductProperties = mempty,
        _upppIdempotencyToken = pIdempotencyToken_
      }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
upppAcceptLanguage :: Lens' UpdateProvisionedProductProperties (Maybe Text)
upppAcceptLanguage = lens _upppAcceptLanguage (\s a -> s {_upppAcceptLanguage = a})

-- | The identifier of the provisioned product.
upppProvisionedProductId :: Lens' UpdateProvisionedProductProperties Text
upppProvisionedProductId = lens _upppProvisionedProductId (\s a -> s {_upppProvisionedProductId = a})

-- | A map that contains the provisioned product properties to be updated. The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an administrator to call @UpdateProvisionedProductProperties@ to update the launch role that is associated with a provisioned product. This role is used when an end user calls a provisioning operation such as @UpdateProvisionedProduct@ , @TerminateProvisionedProduct@ , or @ExecuteProvisionedProductServiceAction@ . Only a role ARN is valid. A user ARN is invalid.  The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user that has permission to see, update, terminate, and execute service actions in the provisioned product. The administrator can change the owner of a provisioned product to another IAM user within the same account. Both end user owners and administrators can see ownership history of the provisioned product using the @ListRecordHistory@ API. The new owner can describe all past records for the provisioned product using the @DescribeRecord@ API. The previous owner can no longer use @DescribeRecord@ , but can still see the product's history from when he was an owner using @ListRecordHistory@ . If a provisioned product ownership is assigned to an end user, they can see and perform any action through the API or Service Catalog console such as update, terminate, and execute service actions. If an end user provisions a product and the owner is updated to someone else, they will no longer be able to see or perform any actions through API or the Service Catalog console on that provisioned product.
upppProvisionedProductProperties :: Lens' UpdateProvisionedProductProperties (HashMap PropertyKey (Text))
upppProvisionedProductProperties = lens _upppProvisionedProductProperties (\s a -> s {_upppProvisionedProductProperties = a}) . _Map

-- | The idempotency token that uniquely identifies the provisioning product update request.
upppIdempotencyToken :: Lens' UpdateProvisionedProductProperties Text
upppIdempotencyToken = lens _upppIdempotencyToken (\s a -> s {_upppIdempotencyToken = a})

instance AWSRequest UpdateProvisionedProductProperties where
  type
    Rs UpdateProvisionedProductProperties =
      UpdateProvisionedProductPropertiesResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          UpdateProvisionedProductPropertiesResponse'
            <$> (x .?> "Status")
            <*> (x .?> "ProvisionedProductProperties" .!@ mempty)
            <*> (x .?> "RecordId")
            <*> (x .?> "ProvisionedProductId")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateProvisionedProductProperties

instance NFData UpdateProvisionedProductProperties

instance ToHeaders UpdateProvisionedProductProperties where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.UpdateProvisionedProductProperties" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateProvisionedProductProperties where
  toJSON UpdateProvisionedProductProperties' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _upppAcceptLanguage,
            Just ("ProvisionedProductId" .= _upppProvisionedProductId),
            Just
              ( "ProvisionedProductProperties"
                  .= _upppProvisionedProductProperties
              ),
            Just ("IdempotencyToken" .= _upppIdempotencyToken)
          ]
      )

instance ToPath UpdateProvisionedProductProperties where
  toPath = const "/"

instance ToQuery UpdateProvisionedProductProperties where
  toQuery = const mempty

-- | /See:/ 'updateProvisionedProductPropertiesResponse' smart constructor.
data UpdateProvisionedProductPropertiesResponse = UpdateProvisionedProductPropertiesResponse'
  { _uppprsStatus ::
      !( Maybe
           RecordStatus
       ),
    _uppprsProvisionedProductProperties ::
      !( Maybe
           ( Map
               PropertyKey
               (Text)
           )
       ),
    _uppprsRecordId ::
      !( Maybe
           Text
       ),
    _uppprsProvisionedProductId ::
      !( Maybe
           Text
       ),
    _uppprsResponseStatus ::
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

-- | Creates a value of 'UpdateProvisionedProductPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppprsStatus' - The status of the request.
--
-- * 'uppprsProvisionedProductProperties' - A map that contains the properties updated.
--
-- * 'uppprsRecordId' - The identifier of the record.
--
-- * 'uppprsProvisionedProductId' - The provisioned product identifier.
--
-- * 'uppprsResponseStatus' - -- | The response status code.
updateProvisionedProductPropertiesResponse ::
  -- | 'uppprsResponseStatus'
  Int ->
  UpdateProvisionedProductPropertiesResponse
updateProvisionedProductPropertiesResponse pResponseStatus_ =
  UpdateProvisionedProductPropertiesResponse'
    { _uppprsStatus =
        Nothing,
      _uppprsProvisionedProductProperties = Nothing,
      _uppprsRecordId = Nothing,
      _uppprsProvisionedProductId = Nothing,
      _uppprsResponseStatus = pResponseStatus_
    }

-- | The status of the request.
uppprsStatus :: Lens' UpdateProvisionedProductPropertiesResponse (Maybe RecordStatus)
uppprsStatus = lens _uppprsStatus (\s a -> s {_uppprsStatus = a})

-- | A map that contains the properties updated.
uppprsProvisionedProductProperties :: Lens' UpdateProvisionedProductPropertiesResponse (HashMap PropertyKey (Text))
uppprsProvisionedProductProperties = lens _uppprsProvisionedProductProperties (\s a -> s {_uppprsProvisionedProductProperties = a}) . _Default . _Map

-- | The identifier of the record.
uppprsRecordId :: Lens' UpdateProvisionedProductPropertiesResponse (Maybe Text)
uppprsRecordId = lens _uppprsRecordId (\s a -> s {_uppprsRecordId = a})

-- | The provisioned product identifier.
uppprsProvisionedProductId :: Lens' UpdateProvisionedProductPropertiesResponse (Maybe Text)
uppprsProvisionedProductId = lens _uppprsProvisionedProductId (\s a -> s {_uppprsProvisionedProductId = a})

-- | -- | The response status code.
uppprsResponseStatus :: Lens' UpdateProvisionedProductPropertiesResponse Int
uppprsResponseStatus = lens _uppprsResponseStatus (\s a -> s {_uppprsResponseStatus = a})

instance NFData UpdateProvisionedProductPropertiesResponse
