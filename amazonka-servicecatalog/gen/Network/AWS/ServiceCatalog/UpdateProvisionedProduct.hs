{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
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
--
module Network.AWS.ServiceCatalog.UpdateProvisionedProduct
    (
    -- * Creating a Request
      updateProvisionedProduct
    , UpdateProvisionedProduct
    -- * Request Lenses
    , uppProvisionedProductName
    , uppProvisioningArtifactId
    , uppAcceptLanguage
    , uppPathId
    , uppProvisioningParameters
    , uppProvisionedProductId
    , uppProductId
    , uppUpdateToken

    -- * Destructuring the Response
    , updateProvisionedProductResponse
    , UpdateProvisionedProductResponse
    -- * Response Lenses
    , upprsRecordDetail
    , upprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'updateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { _uppProvisionedProductName :: !(Maybe Text)
  , _uppProvisioningArtifactId :: !(Maybe Text)
  , _uppAcceptLanguage         :: !(Maybe Text)
  , _uppPathId                 :: !(Maybe Text)
  , _uppProvisioningParameters :: !(Maybe [UpdateProvisioningParameter])
  , _uppProvisionedProductId   :: !(Maybe Text)
  , _uppProductId              :: !(Maybe Text)
  , _uppUpdateToken            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppProvisionedProductName' - The updated name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- * 'uppProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'uppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'uppPathId' - The new path identifier. This value is optional if the product has a default path, and required if the product has more than one path.
--
-- * 'uppProvisioningParameters' - The new parameters.
--
-- * 'uppProvisionedProductId' - The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- * 'uppProductId' - The identifier of the provisioned product.
--
-- * 'uppUpdateToken' - The idempotency token that uniquely identifies the provisioning update request.
updateProvisionedProduct
    :: Text -- ^ 'uppUpdateToken'
    -> UpdateProvisionedProduct
updateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { _uppProvisionedProductName = Nothing
    , _uppProvisioningArtifactId = Nothing
    , _uppAcceptLanguage = Nothing
    , _uppPathId = Nothing
    , _uppProvisioningParameters = Nothing
    , _uppProvisionedProductId = Nothing
    , _uppProductId = Nothing
    , _uppUpdateToken = pUpdateToken_
    }


-- | The updated name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
uppProvisionedProductName :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisionedProductName = lens _uppProvisionedProductName (\ s a -> s{_uppProvisionedProductName = a})

-- | The identifier of the provisioning artifact.
uppProvisioningArtifactId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisioningArtifactId = lens _uppProvisioningArtifactId (\ s a -> s{_uppProvisioningArtifactId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
uppAcceptLanguage :: Lens' UpdateProvisionedProduct (Maybe Text)
uppAcceptLanguage = lens _uppAcceptLanguage (\ s a -> s{_uppAcceptLanguage = a})

-- | The new path identifier. This value is optional if the product has a default path, and required if the product has more than one path.
uppPathId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppPathId = lens _uppPathId (\ s a -> s{_uppPathId = a})

-- | The new parameters.
uppProvisioningParameters :: Lens' UpdateProvisionedProduct [UpdateProvisioningParameter]
uppProvisioningParameters = lens _uppProvisioningParameters (\ s a -> s{_uppProvisioningParameters = a}) . _Default . _Coerce

-- | The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
uppProvisionedProductId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProvisionedProductId = lens _uppProvisionedProductId (\ s a -> s{_uppProvisionedProductId = a})

-- | The identifier of the provisioned product.
uppProductId :: Lens' UpdateProvisionedProduct (Maybe Text)
uppProductId = lens _uppProductId (\ s a -> s{_uppProductId = a})

-- | The idempotency token that uniquely identifies the provisioning update request.
uppUpdateToken :: Lens' UpdateProvisionedProduct Text
uppUpdateToken = lens _uppUpdateToken (\ s a -> s{_uppUpdateToken = a})

instance AWSRequest UpdateProvisionedProduct where
        type Rs UpdateProvisionedProduct =
             UpdateProvisionedProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 UpdateProvisionedProductResponse' <$>
                   (x .?> "RecordDetail") <*> (pure (fromEnum s)))

instance Hashable UpdateProvisionedProduct where

instance NFData UpdateProvisionedProduct where

instance ToHeaders UpdateProvisionedProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.UpdateProvisionedProduct"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProvisionedProduct where
        toJSON UpdateProvisionedProduct'{..}
          = object
              (catMaybes
                 [("ProvisionedProductName" .=) <$>
                    _uppProvisionedProductName,
                  ("ProvisioningArtifactId" .=) <$>
                    _uppProvisioningArtifactId,
                  ("AcceptLanguage" .=) <$> _uppAcceptLanguage,
                  ("PathId" .=) <$> _uppPathId,
                  ("ProvisioningParameters" .=) <$>
                    _uppProvisioningParameters,
                  ("ProvisionedProductId" .=) <$>
                    _uppProvisionedProductId,
                  ("ProductId" .=) <$> _uppProductId,
                  Just ("UpdateToken" .= _uppUpdateToken)])

instance ToPath UpdateProvisionedProduct where
        toPath = const "/"

instance ToQuery UpdateProvisionedProduct where
        toQuery = const mempty

-- | /See:/ 'updateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { _upprsRecordDetail   :: !(Maybe RecordDetail)
  , _upprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upprsRecordDetail' - Information about the result of the request.
--
-- * 'upprsResponseStatus' - -- | The response status code.
updateProvisionedProductResponse
    :: Int -- ^ 'upprsResponseStatus'
    -> UpdateProvisionedProductResponse
updateProvisionedProductResponse pResponseStatus_ =
  UpdateProvisionedProductResponse'
    {_upprsRecordDetail = Nothing, _upprsResponseStatus = pResponseStatus_}


-- | Information about the result of the request.
upprsRecordDetail :: Lens' UpdateProvisionedProductResponse (Maybe RecordDetail)
upprsRecordDetail = lens _upprsRecordDetail (\ s a -> s{_upprsRecordDetail = a})

-- | -- | The response status code.
upprsResponseStatus :: Lens' UpdateProvisionedProductResponse Int
upprsResponseStatus = lens _upprsResponseStatus (\ s a -> s{_upprsResponseStatus = a})

instance NFData UpdateProvisionedProductResponse
         where
