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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the configuration of the specified provisioned product.
--
--
-- If there are tags associated with the object, they cannot be updated or added. Depending on the specific updates requested, this operation can update with no interruption, with some interruption, or replace the provisioned product entirely.
--
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.UpdateProvisionedProduct
  ( -- * Creating a Request
    updateProvisionedProduct,
    UpdateProvisionedProduct,

    -- * Request Lenses
    uppProductName,
    uppProvisionedProductName,
    uppProvisioningArtifactId,
    uppProvisioningArtifactName,
    uppPathName,
    uppAcceptLanguage,
    uppPathId,
    uppProvisioningParameters,
    uppProvisionedProductId,
    uppProductId,
    uppTags,
    uppProvisioningPreferences,
    uppUpdateToken,

    -- * Destructuring the Response
    updateProvisionedProductResponse,
    UpdateProvisionedProductResponse,

    -- * Response Lenses
    upprsRecordDetail,
    upprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'updateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { _uppProductName ::
      !(Maybe Text),
    _uppProvisionedProductName ::
      !(Maybe Text),
    _uppProvisioningArtifactId ::
      !(Maybe Text),
    _uppProvisioningArtifactName ::
      !(Maybe Text),
    _uppPathName :: !(Maybe Text),
    _uppAcceptLanguage :: !(Maybe Text),
    _uppPathId :: !(Maybe Text),
    _uppProvisioningParameters ::
      !(Maybe [UpdateProvisioningParameter]),
    _uppProvisionedProductId :: !(Maybe Text),
    _uppProductId :: !(Maybe Text),
    _uppTags :: !(Maybe [Tag]),
    _uppProvisioningPreferences ::
      !(Maybe UpdateProvisioningPreferences),
    _uppUpdateToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppProductName' - The name of the product. You must provide the name or ID, but not both.
--
-- * 'uppProvisionedProductName' - The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- * 'uppProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'uppProvisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- * 'uppPathName' - The name of the path. You must provide the name or ID, but not both.
--
-- * 'uppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'uppPathId' - The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
--
-- * 'uppProvisioningParameters' - The new parameters.
--
-- * 'uppProvisionedProductId' - The identifier of the provisioned product. You must provide the name or ID, but not both.
--
-- * 'uppProductId' - The identifier of the product. You must provide the name or ID, but not both.
--
-- * 'uppTags' - One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- * 'uppProvisioningPreferences' - An object that contains information about the provisioning preferences for a stack set.
--
-- * 'uppUpdateToken' - The idempotency token that uniquely identifies the provisioning update request.
updateProvisionedProduct ::
  -- | 'uppUpdateToken'
  Text ->
  UpdateProvisionedProduct
updateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { _uppProductName = Nothing,
      _uppProvisionedProductName = Nothing,
      _uppProvisioningArtifactId = Nothing,
      _uppProvisioningArtifactName = Nothing,
      _uppPathName = Nothing,
      _uppAcceptLanguage = Nothing,
      _uppPathId = Nothing,
      _uppProvisioningParameters = Nothing,
      _uppProvisionedProductId = Nothing,
      _uppProductId = Nothing,
      _uppTags = Nothing,
      _uppProvisioningPreferences = Nothing,
      _uppUpdateToken = pUpdateToken_
    }

-- | The name of the product. You must provide the name or ID, but not both.
uppProductName :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProductName = lens _uppProductName (\s a -> s {_uppProductName = a})

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
uppProvisionedProductName :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisionedProductName = lens _uppProvisionedProductName (\s a -> s {_uppProvisionedProductName = a})

-- | The identifier of the provisioning artifact.
uppProvisioningArtifactId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisioningArtifactId = lens _uppProvisioningArtifactId (\s a -> s {_uppProvisioningArtifactId = a})

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
uppProvisioningArtifactName :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisioningArtifactName = lens _uppProvisioningArtifactName (\s a -> s {_uppProvisioningArtifactName = a})

-- | The name of the path. You must provide the name or ID, but not both.
uppPathName :: Lens' UpdateProvisionedProduct (Maybe Text)
uppPathName = lens _uppPathName (\s a -> s {_uppPathName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
uppAcceptLanguage :: Lens' UpdateProvisionedProduct (Maybe Text)
uppAcceptLanguage = lens _uppAcceptLanguage (\s a -> s {_uppAcceptLanguage = a})

-- | The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
uppPathId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppPathId = lens _uppPathId (\s a -> s {_uppPathId = a})

-- | The new parameters.
uppProvisioningParameters :: Lens' UpdateProvisionedProduct [UpdateProvisioningParameter]
uppProvisioningParameters = lens _uppProvisioningParameters (\s a -> s {_uppProvisioningParameters = a}) . _Default . _Coerce

-- | The identifier of the provisioned product. You must provide the name or ID, but not both.
uppProvisionedProductId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisionedProductId = lens _uppProvisionedProductId (\s a -> s {_uppProvisionedProductId = a})

-- | The identifier of the product. You must provide the name or ID, but not both.
uppProductId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProductId = lens _uppProductId (\s a -> s {_uppProductId = a})

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
uppTags :: Lens' UpdateProvisionedProduct [Tag]
uppTags = lens _uppTags (\s a -> s {_uppTags = a}) . _Default . _Coerce

-- | An object that contains information about the provisioning preferences for a stack set.
uppProvisioningPreferences :: Lens' UpdateProvisionedProduct (Maybe UpdateProvisioningPreferences)
uppProvisioningPreferences = lens _uppProvisioningPreferences (\s a -> s {_uppProvisioningPreferences = a})

-- | The idempotency token that uniquely identifies the provisioning update request.
uppUpdateToken :: Lens' UpdateProvisionedProduct Text
uppUpdateToken = lens _uppUpdateToken (\s a -> s {_uppUpdateToken = a})

instance AWSRequest UpdateProvisionedProduct where
  type Rs UpdateProvisionedProduct = UpdateProvisionedProductResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          UpdateProvisionedProductResponse'
            <$> (x .?> "RecordDetail") <*> (pure (fromEnum s))
      )

instance Hashable UpdateProvisionedProduct

instance NFData UpdateProvisionedProduct

instance ToHeaders UpdateProvisionedProduct where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.UpdateProvisionedProduct" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateProvisionedProduct where
  toJSON UpdateProvisionedProduct' {..} =
    object
      ( catMaybes
          [ ("ProductName" .=) <$> _uppProductName,
            ("ProvisionedProductName" .=) <$> _uppProvisionedProductName,
            ("ProvisioningArtifactId" .=) <$> _uppProvisioningArtifactId,
            ("ProvisioningArtifactName" .=) <$> _uppProvisioningArtifactName,
            ("PathName" .=) <$> _uppPathName,
            ("AcceptLanguage" .=) <$> _uppAcceptLanguage,
            ("PathId" .=) <$> _uppPathId,
            ("ProvisioningParameters" .=) <$> _uppProvisioningParameters,
            ("ProvisionedProductId" .=) <$> _uppProvisionedProductId,
            ("ProductId" .=) <$> _uppProductId,
            ("Tags" .=) <$> _uppTags,
            ("ProvisioningPreferences" .=) <$> _uppProvisioningPreferences,
            Just ("UpdateToken" .= _uppUpdateToken)
          ]
      )

instance ToPath UpdateProvisionedProduct where
  toPath = const "/"

instance ToQuery UpdateProvisionedProduct where
  toQuery = const mempty

-- | /See:/ 'updateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { _upprsRecordDetail ::
      !(Maybe RecordDetail),
    _upprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upprsRecordDetail' - Information about the result of the request.
--
-- * 'upprsResponseStatus' - -- | The response status code.
updateProvisionedProductResponse ::
  -- | 'upprsResponseStatus'
  Int ->
  UpdateProvisionedProductResponse
updateProvisionedProductResponse pResponseStatus_ =
  UpdateProvisionedProductResponse'
    { _upprsRecordDetail = Nothing,
      _upprsResponseStatus = pResponseStatus_
    }

-- | Information about the result of the request.
upprsRecordDetail :: Lens' UpdateProvisionedProductResponse (Maybe RecordDetail)
upprsRecordDetail = lens _upprsRecordDetail (\s a -> s {_upprsRecordDetail = a})

-- | -- | The response status code.
upprsResponseStatus :: Lens' UpdateProvisionedProductResponse Int
upprsResponseStatus = lens _upprsResponseStatus (\s a -> s {_upprsResponseStatus = a})

instance NFData UpdateProvisionedProductResponse
