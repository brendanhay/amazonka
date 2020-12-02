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
-- Module      : Network.AWS.ServiceCatalog.UpdateProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified product.
--
--
module Network.AWS.ServiceCatalog.UpdateProduct
    (
    -- * Creating a Request
      updateProduct
    , UpdateProduct
    -- * Request Lenses
    , upRemoveTags
    , upOwner
    , upSupportURL
    , upDistributor
    , upName
    , upAcceptLanguage
    , upAddTags
    , upSupportEmail
    , upDescription
    , upSupportDescription
    , upId

    -- * Destructuring the Response
    , updateProductResponse
    , UpdateProductResponse
    -- * Response Lenses
    , ursProductViewDetail
    , ursTags
    , ursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'updateProduct' smart constructor.
data UpdateProduct = UpdateProduct'
  { _upRemoveTags         :: !(Maybe [Text])
  , _upOwner              :: !(Maybe Text)
  , _upSupportURL         :: !(Maybe Text)
  , _upDistributor        :: !(Maybe Text)
  , _upName               :: !(Maybe Text)
  , _upAcceptLanguage     :: !(Maybe Text)
  , _upAddTags            :: !(Maybe [Tag])
  , _upSupportEmail       :: !(Maybe Text)
  , _upDescription        :: !(Maybe Text)
  , _upSupportDescription :: !(Maybe Text)
  , _upId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upRemoveTags' - The tags to remove from the product.
--
-- * 'upOwner' - The updated owner of the product.
--
-- * 'upSupportURL' - The updated support URL for the product.
--
-- * 'upDistributor' - The updated distributor of the product.
--
-- * 'upName' - The updated product name.
--
-- * 'upAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'upAddTags' - The tags to add to the product.
--
-- * 'upSupportEmail' - The updated support email for the product.
--
-- * 'upDescription' - The updated description of the product.
--
-- * 'upSupportDescription' - The updated support description for the product.
--
-- * 'upId' - The product identifier.
updateProduct
    :: Text -- ^ 'upId'
    -> UpdateProduct
updateProduct pId_ =
  UpdateProduct'
    { _upRemoveTags = Nothing
    , _upOwner = Nothing
    , _upSupportURL = Nothing
    , _upDistributor = Nothing
    , _upName = Nothing
    , _upAcceptLanguage = Nothing
    , _upAddTags = Nothing
    , _upSupportEmail = Nothing
    , _upDescription = Nothing
    , _upSupportDescription = Nothing
    , _upId = pId_
    }


-- | The tags to remove from the product.
upRemoveTags :: Lens' UpdateProduct [Text]
upRemoveTags = lens _upRemoveTags (\ s a -> s{_upRemoveTags = a}) . _Default . _Coerce

-- | The updated owner of the product.
upOwner :: Lens' UpdateProduct (Maybe Text)
upOwner = lens _upOwner (\ s a -> s{_upOwner = a})

-- | The updated support URL for the product.
upSupportURL :: Lens' UpdateProduct (Maybe Text)
upSupportURL = lens _upSupportURL (\ s a -> s{_upSupportURL = a})

-- | The updated distributor of the product.
upDistributor :: Lens' UpdateProduct (Maybe Text)
upDistributor = lens _upDistributor (\ s a -> s{_upDistributor = a})

-- | The updated product name.
upName :: Lens' UpdateProduct (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
upAcceptLanguage :: Lens' UpdateProduct (Maybe Text)
upAcceptLanguage = lens _upAcceptLanguage (\ s a -> s{_upAcceptLanguage = a})

-- | The tags to add to the product.
upAddTags :: Lens' UpdateProduct [Tag]
upAddTags = lens _upAddTags (\ s a -> s{_upAddTags = a}) . _Default . _Coerce

-- | The updated support email for the product.
upSupportEmail :: Lens' UpdateProduct (Maybe Text)
upSupportEmail = lens _upSupportEmail (\ s a -> s{_upSupportEmail = a})

-- | The updated description of the product.
upDescription :: Lens' UpdateProduct (Maybe Text)
upDescription = lens _upDescription (\ s a -> s{_upDescription = a})

-- | The updated support description for the product.
upSupportDescription :: Lens' UpdateProduct (Maybe Text)
upSupportDescription = lens _upSupportDescription (\ s a -> s{_upSupportDescription = a})

-- | The product identifier.
upId :: Lens' UpdateProduct Text
upId = lens _upId (\ s a -> s{_upId = a})

instance AWSRequest UpdateProduct where
        type Rs UpdateProduct = UpdateProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 UpdateProductResponse' <$>
                   (x .?> "ProductViewDetail") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdateProduct where

instance NFData UpdateProduct where

instance ToHeaders UpdateProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.UpdateProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProduct where
        toJSON UpdateProduct'{..}
          = object
              (catMaybes
                 [("RemoveTags" .=) <$> _upRemoveTags,
                  ("Owner" .=) <$> _upOwner,
                  ("SupportUrl" .=) <$> _upSupportURL,
                  ("Distributor" .=) <$> _upDistributor,
                  ("Name" .=) <$> _upName,
                  ("AcceptLanguage" .=) <$> _upAcceptLanguage,
                  ("AddTags" .=) <$> _upAddTags,
                  ("SupportEmail" .=) <$> _upSupportEmail,
                  ("Description" .=) <$> _upDescription,
                  ("SupportDescription" .=) <$> _upSupportDescription,
                  Just ("Id" .= _upId)])

instance ToPath UpdateProduct where
        toPath = const "/"

instance ToQuery UpdateProduct where
        toQuery = const mempty

-- | /See:/ 'updateProductResponse' smart constructor.
data UpdateProductResponse = UpdateProductResponse'
  { _ursProductViewDetail :: !(Maybe ProductViewDetail)
  , _ursTags              :: !(Maybe [Tag])
  , _ursResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursProductViewDetail' - Information about the product view.
--
-- * 'ursTags' - Information about the tags associated with the product.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateProductResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UpdateProductResponse
updateProductResponse pResponseStatus_ =
  UpdateProductResponse'
    { _ursProductViewDetail = Nothing
    , _ursTags = Nothing
    , _ursResponseStatus = pResponseStatus_
    }


-- | Information about the product view.
ursProductViewDetail :: Lens' UpdateProductResponse (Maybe ProductViewDetail)
ursProductViewDetail = lens _ursProductViewDetail (\ s a -> s{_ursProductViewDetail = a})

-- | Information about the tags associated with the product.
ursTags :: Lens' UpdateProductResponse [Tag]
ursTags = lens _ursTags (\ s a -> s{_ursTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateProductResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UpdateProductResponse where
