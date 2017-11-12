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
-- Module      : Network.AWS.ServiceCatalog.CreateProduct
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new product.
--
--
module Network.AWS.ServiceCatalog.CreateProduct
    (
    -- * Creating a Request
      createProduct
    , CreateProduct
    -- * Request Lenses
    , cpSupportURL
    , cpDistributor
    , cpAcceptLanguage
    , cpSupportEmail
    , cpDescription
    , cpTags
    , cpSupportDescription
    , cpName
    , cpOwner
    , cpProductType
    , cpProvisioningArtifactParameters
    , cpIdempotencyToken

    -- * Destructuring the Response
    , createProductResponse
    , CreateProductResponse
    -- * Response Lenses
    , cprsProductViewDetail
    , cprsProvisioningArtifactDetail
    , cprsTags
    , cprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createProduct' smart constructor.
data CreateProduct = CreateProduct'
  { _cpSupportURL                     :: !(Maybe Text)
  , _cpDistributor                    :: !(Maybe Text)
  , _cpAcceptLanguage                 :: !(Maybe Text)
  , _cpSupportEmail                   :: !(Maybe Text)
  , _cpDescription                    :: !(Maybe Text)
  , _cpTags                           :: !(Maybe [Tag])
  , _cpSupportDescription             :: !(Maybe Text)
  , _cpName                           :: !Text
  , _cpOwner                          :: !Text
  , _cpProductType                    :: !ProductType
  , _cpProvisioningArtifactParameters :: !ProvisioningArtifactProperties
  , _cpIdempotencyToken               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSupportURL' - Contact URL for product support.
--
-- * 'cpDistributor' - The distributor of the product.
--
-- * 'cpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'cpSupportEmail' - Contact email for product support.
--
-- * 'cpDescription' - The text description of the product.
--
-- * 'cpTags' - Tags to associate with the new product.
--
-- * 'cpSupportDescription' - Support information about the product.
--
-- * 'cpName' - The name of the product.
--
-- * 'cpOwner' - The owner of the product.
--
-- * 'cpProductType' - The type of the product to create.
--
-- * 'cpProvisioningArtifactParameters' - Parameters for the provisioning artifact.
--
-- * 'cpIdempotencyToken' - A token to disambiguate duplicate requests. You can use the same input in multiple requests, provided that you also specify a different idempotency token for each request.
createProduct
    :: Text -- ^ 'cpName'
    -> Text -- ^ 'cpOwner'
    -> ProductType -- ^ 'cpProductType'
    -> ProvisioningArtifactProperties -- ^ 'cpProvisioningArtifactParameters'
    -> Text -- ^ 'cpIdempotencyToken'
    -> CreateProduct
createProduct pName_ pOwner_ pProductType_ pProvisioningArtifactParameters_ pIdempotencyToken_ =
  CreateProduct'
  { _cpSupportURL = Nothing
  , _cpDistributor = Nothing
  , _cpAcceptLanguage = Nothing
  , _cpSupportEmail = Nothing
  , _cpDescription = Nothing
  , _cpTags = Nothing
  , _cpSupportDescription = Nothing
  , _cpName = pName_
  , _cpOwner = pOwner_
  , _cpProductType = pProductType_
  , _cpProvisioningArtifactParameters = pProvisioningArtifactParameters_
  , _cpIdempotencyToken = pIdempotencyToken_
  }


-- | Contact URL for product support.
cpSupportURL :: Lens' CreateProduct (Maybe Text)
cpSupportURL = lens _cpSupportURL (\ s a -> s{_cpSupportURL = a});

-- | The distributor of the product.
cpDistributor :: Lens' CreateProduct (Maybe Text)
cpDistributor = lens _cpDistributor (\ s a -> s{_cpDistributor = a});

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
cpAcceptLanguage :: Lens' CreateProduct (Maybe Text)
cpAcceptLanguage = lens _cpAcceptLanguage (\ s a -> s{_cpAcceptLanguage = a});

-- | Contact email for product support.
cpSupportEmail :: Lens' CreateProduct (Maybe Text)
cpSupportEmail = lens _cpSupportEmail (\ s a -> s{_cpSupportEmail = a});

-- | The text description of the product.
cpDescription :: Lens' CreateProduct (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a});

-- | Tags to associate with the new product.
cpTags :: Lens' CreateProduct [Tag]
cpTags = lens _cpTags (\ s a -> s{_cpTags = a}) . _Default . _Coerce;

-- | Support information about the product.
cpSupportDescription :: Lens' CreateProduct (Maybe Text)
cpSupportDescription = lens _cpSupportDescription (\ s a -> s{_cpSupportDescription = a});

-- | The name of the product.
cpName :: Lens' CreateProduct Text
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | The owner of the product.
cpOwner :: Lens' CreateProduct Text
cpOwner = lens _cpOwner (\ s a -> s{_cpOwner = a});

-- | The type of the product to create.
cpProductType :: Lens' CreateProduct ProductType
cpProductType = lens _cpProductType (\ s a -> s{_cpProductType = a});

-- | Parameters for the provisioning artifact.
cpProvisioningArtifactParameters :: Lens' CreateProduct ProvisioningArtifactProperties
cpProvisioningArtifactParameters = lens _cpProvisioningArtifactParameters (\ s a -> s{_cpProvisioningArtifactParameters = a});

-- | A token to disambiguate duplicate requests. You can use the same input in multiple requests, provided that you also specify a different idempotency token for each request.
cpIdempotencyToken :: Lens' CreateProduct Text
cpIdempotencyToken = lens _cpIdempotencyToken (\ s a -> s{_cpIdempotencyToken = a});

instance AWSRequest CreateProduct where
        type Rs CreateProduct = CreateProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreateProductResponse' <$>
                   (x .?> "ProductViewDetail") <*>
                     (x .?> "ProvisioningArtifactDetail")
                     <*> (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateProduct where

instance NFData CreateProduct where

instance ToHeaders CreateProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreateProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProduct where
        toJSON CreateProduct'{..}
          = object
              (catMaybes
                 [("SupportUrl" .=) <$> _cpSupportURL,
                  ("Distributor" .=) <$> _cpDistributor,
                  ("AcceptLanguage" .=) <$> _cpAcceptLanguage,
                  ("SupportEmail" .=) <$> _cpSupportEmail,
                  ("Description" .=) <$> _cpDescription,
                  ("Tags" .=) <$> _cpTags,
                  ("SupportDescription" .=) <$> _cpSupportDescription,
                  Just ("Name" .= _cpName), Just ("Owner" .= _cpOwner),
                  Just ("ProductType" .= _cpProductType),
                  Just
                    ("ProvisioningArtifactParameters" .=
                       _cpProvisioningArtifactParameters),
                  Just ("IdempotencyToken" .= _cpIdempotencyToken)])

instance ToPath CreateProduct where
        toPath = const "/"

instance ToQuery CreateProduct where
        toQuery = const mempty

-- | /See:/ 'createProductResponse' smart constructor.
data CreateProductResponse = CreateProductResponse'
  { _cprsProductViewDetail          :: !(Maybe ProductViewDetail)
  , _cprsProvisioningArtifactDetail :: !(Maybe ProvisioningArtifactDetail)
  , _cprsTags                       :: !(Maybe [Tag])
  , _cprsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProductViewDetail' - The resulting detailed product view information.
--
-- * 'cprsProvisioningArtifactDetail' - The resulting detailed provisioning artifact information.
--
-- * 'cprsTags' - Tags successfully associated with the new product.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProductResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreateProductResponse
createProductResponse pResponseStatus_ =
  CreateProductResponse'
  { _cprsProductViewDetail = Nothing
  , _cprsProvisioningArtifactDetail = Nothing
  , _cprsTags = Nothing
  , _cprsResponseStatus = pResponseStatus_
  }


-- | The resulting detailed product view information.
cprsProductViewDetail :: Lens' CreateProductResponse (Maybe ProductViewDetail)
cprsProductViewDetail = lens _cprsProductViewDetail (\ s a -> s{_cprsProductViewDetail = a});

-- | The resulting detailed provisioning artifact information.
cprsProvisioningArtifactDetail :: Lens' CreateProductResponse (Maybe ProvisioningArtifactDetail)
cprsProvisioningArtifactDetail = lens _cprsProvisioningArtifactDetail (\ s a -> s{_cprsProvisioningArtifactDetail = a});

-- | Tags successfully associated with the new product.
cprsTags :: Lens' CreateProductResponse [Tag]
cprsTags = lens _cprsTags (\ s a -> s{_cprsTags = a}) . _Default . _Coerce;

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProductResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a});

instance NFData CreateProductResponse where
