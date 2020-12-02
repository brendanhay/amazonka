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
-- Module      : Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the import of a resource as a Service Catalog provisioned product that is associated to a Service Catalog product and provisioning artifact. Once imported all supported Service Catalog governance actions are supported on the provisioned product.
--
--
-- Resource import only supports CloudFormation stack ARNs. CloudFormation StackSets and non-root nested stacks are not supported.
--
-- The CloudFormation stack must have one of the following statuses to be imported: CREATE_COMPLETE, UPDATE_COMPLETE, UPDATE_ROLLBACK_COMPLETE, IMPORT_COMPLETE, IMPORT_ROLLBACK_COMPLETE.
--
-- Import of the resource requires that the CloudFormation stack template matches the associated Service Catalog product provisioning artifact.
module Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
  ( -- * Creating a Request
    importAsProvisionedProduct,
    ImportAsProvisionedProduct,

    -- * Request Lenses
    iappAcceptLanguage,
    iappProductId,
    iappProvisioningArtifactId,
    iappProvisionedProductName,
    iappPhysicalId,
    iappIdempotencyToken,

    -- * Destructuring the Response
    importAsProvisionedProductResponse,
    ImportAsProvisionedProductResponse,

    -- * Response Lenses
    iapprsRecordDetail,
    iapprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'importAsProvisionedProduct' smart constructor.
data ImportAsProvisionedProduct = ImportAsProvisionedProduct'
  { _iappAcceptLanguage ::
      !(Maybe Text),
    _iappProductId :: !Text,
    _iappProvisioningArtifactId :: !Text,
    _iappProvisionedProductName :: !Text,
    _iappPhysicalId :: !Text,
    _iappIdempotencyToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportAsProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iappAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'iappProductId' - The product identifier.
--
-- * 'iappProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'iappProvisionedProductName' - The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned.
--
-- * 'iappPhysicalId' - The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
--
-- * 'iappIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
importAsProvisionedProduct ::
  -- | 'iappProductId'
  Text ->
  -- | 'iappProvisioningArtifactId'
  Text ->
  -- | 'iappProvisionedProductName'
  Text ->
  -- | 'iappPhysicalId'
  Text ->
  -- | 'iappIdempotencyToken'
  Text ->
  ImportAsProvisionedProduct
importAsProvisionedProduct
  pProductId_
  pProvisioningArtifactId_
  pProvisionedProductName_
  pPhysicalId_
  pIdempotencyToken_ =
    ImportAsProvisionedProduct'
      { _iappAcceptLanguage = Nothing,
        _iappProductId = pProductId_,
        _iappProvisioningArtifactId = pProvisioningArtifactId_,
        _iappProvisionedProductName = pProvisionedProductName_,
        _iappPhysicalId = pPhysicalId_,
        _iappIdempotencyToken = pIdempotencyToken_
      }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
iappAcceptLanguage :: Lens' ImportAsProvisionedProduct (Maybe Text)
iappAcceptLanguage = lens _iappAcceptLanguage (\s a -> s {_iappAcceptLanguage = a})

-- | The product identifier.
iappProductId :: Lens' ImportAsProvisionedProduct Text
iappProductId = lens _iappProductId (\s a -> s {_iappProductId = a})

-- | The identifier of the provisioning artifact.
iappProvisioningArtifactId :: Lens' ImportAsProvisionedProduct Text
iappProvisioningArtifactId = lens _iappProvisioningArtifactId (\s a -> s {_iappProvisioningArtifactId = a})

-- | The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned.
iappProvisionedProductName :: Lens' ImportAsProvisionedProduct Text
iappProvisionedProductName = lens _iappProvisionedProductName (\s a -> s {_iappProvisionedProductName = a})

-- | The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
iappPhysicalId :: Lens' ImportAsProvisionedProduct Text
iappPhysicalId = lens _iappPhysicalId (\s a -> s {_iappPhysicalId = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
iappIdempotencyToken :: Lens' ImportAsProvisionedProduct Text
iappIdempotencyToken = lens _iappIdempotencyToken (\s a -> s {_iappIdempotencyToken = a})

instance AWSRequest ImportAsProvisionedProduct where
  type
    Rs ImportAsProvisionedProduct =
      ImportAsProvisionedProductResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ImportAsProvisionedProductResponse'
            <$> (x .?> "RecordDetail") <*> (pure (fromEnum s))
      )

instance Hashable ImportAsProvisionedProduct

instance NFData ImportAsProvisionedProduct

instance ToHeaders ImportAsProvisionedProduct where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.ImportAsProvisionedProduct" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportAsProvisionedProduct where
  toJSON ImportAsProvisionedProduct' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _iappAcceptLanguage,
            Just ("ProductId" .= _iappProductId),
            Just ("ProvisioningArtifactId" .= _iappProvisioningArtifactId),
            Just ("ProvisionedProductName" .= _iappProvisionedProductName),
            Just ("PhysicalId" .= _iappPhysicalId),
            Just ("IdempotencyToken" .= _iappIdempotencyToken)
          ]
      )

instance ToPath ImportAsProvisionedProduct where
  toPath = const "/"

instance ToQuery ImportAsProvisionedProduct where
  toQuery = const mempty

-- | /See:/ 'importAsProvisionedProductResponse' smart constructor.
data ImportAsProvisionedProductResponse = ImportAsProvisionedProductResponse'
  { _iapprsRecordDetail ::
      !(Maybe RecordDetail),
    _iapprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportAsProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapprsRecordDetail' - Undocumented member.
--
-- * 'iapprsResponseStatus' - -- | The response status code.
importAsProvisionedProductResponse ::
  -- | 'iapprsResponseStatus'
  Int ->
  ImportAsProvisionedProductResponse
importAsProvisionedProductResponse pResponseStatus_ =
  ImportAsProvisionedProductResponse'
    { _iapprsRecordDetail =
        Nothing,
      _iapprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
iapprsRecordDetail :: Lens' ImportAsProvisionedProductResponse (Maybe RecordDetail)
iapprsRecordDetail = lens _iapprsRecordDetail (\s a -> s {_iapprsRecordDetail = a})

-- | -- | The response status code.
iapprsResponseStatus :: Lens' ImportAsProvisionedProductResponse Int
iapprsResponseStatus = lens _iapprsResponseStatus (\s a -> s {_iapprsResponseStatus = a})

instance NFData ImportAsProvisionedProductResponse
