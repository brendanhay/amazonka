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
-- Module      : Network.AWS.ServiceCatalog.DeleteProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified product.
--
--
-- You cannot delete a product if it was shared with you or is associated with a portfolio.
--
module Network.AWS.ServiceCatalog.DeleteProduct
    (
    -- * Creating a Request
      deleteProduct
    , DeleteProduct
    -- * Request Lenses
    , dppAcceptLanguage
    , dppId

    -- * Destructuring the Response
    , deleteProductResponse
    , DeleteProductResponse
    -- * Response Lenses
    , delersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'
  { _dppAcceptLanguage :: !(Maybe Text)
  , _dppId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dppId' - The product identifier.
deleteProduct
    :: Text -- ^ 'dppId'
    -> DeleteProduct
deleteProduct pId_ =
  DeleteProduct' {_dppAcceptLanguage = Nothing, _dppId = pId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dppAcceptLanguage :: Lens' DeleteProduct (Maybe Text)
dppAcceptLanguage = lens _dppAcceptLanguage (\ s a -> s{_dppAcceptLanguage = a})

-- | The product identifier.
dppId :: Lens' DeleteProduct Text
dppId = lens _dppId (\ s a -> s{_dppId = a})

instance AWSRequest DeleteProduct where
        type Rs DeleteProduct = DeleteProductResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProductResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteProduct where

instance NFData DeleteProduct where

instance ToHeaders DeleteProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProduct where
        toJSON DeleteProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dppAcceptLanguage,
                  Just ("Id" .= _dppId)])

instance ToPath DeleteProduct where
        toPath = const "/"

instance ToQuery DeleteProduct where
        toQuery = const mempty

-- | /See:/ 'deleteProductResponse' smart constructor.
newtype DeleteProductResponse = DeleteProductResponse'
  { _delersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delersResponseStatus' - -- | The response status code.
deleteProductResponse
    :: Int -- ^ 'delersResponseStatus'
    -> DeleteProductResponse
deleteProductResponse pResponseStatus_ =
  DeleteProductResponse' {_delersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delersResponseStatus :: Lens' DeleteProductResponse Int
delersResponseStatus = lens _delersResponseStatus (\ s a -> s{_delersResponseStatus = a})

instance NFData DeleteProductResponse where
