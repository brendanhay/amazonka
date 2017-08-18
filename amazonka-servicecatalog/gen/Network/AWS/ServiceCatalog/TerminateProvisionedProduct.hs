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
-- Module      : Network.AWS.ServiceCatalog.TerminateProvisionedProduct
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests termination of an existing ProvisionedProduct object. If there are @Tags@ associated with the object, they are terminated when the ProvisionedProduct object is terminated.
--
--
-- This operation does not delete any records associated with the ProvisionedProduct object.
--
-- You can check the status of this request using the 'DescribeRecord' operation.
--
module Network.AWS.ServiceCatalog.TerminateProvisionedProduct
    (
    -- * Creating a Request
      terminateProvisionedProduct
    , TerminateProvisionedProduct
    -- * Request Lenses
    , tppProvisionedProductName
    , tppAcceptLanguage
    , tppIgnoreErrors
    , tppProvisionedProductId
    , tppTerminateToken

    -- * Destructuring the Response
    , terminateProvisionedProductResponse
    , TerminateProvisionedProductResponse
    -- * Response Lenses
    , tpprsRecordDetail
    , tpprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'terminateProvisionedProduct' smart constructor.
data TerminateProvisionedProduct = TerminateProvisionedProduct'
    { _tppProvisionedProductName :: !(Maybe Text)
    , _tppAcceptLanguage         :: !(Maybe Text)
    , _tppIgnoreErrors           :: !(Maybe Bool)
    , _tppProvisionedProductId   :: !(Maybe Text)
    , _tppTerminateToken         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tppProvisionedProductName' - The name of the ProvisionedProduct object to terminate. Specify either @ProvisionedProductName@ or @ProvisionedProductId@ , but not both.
--
-- * 'tppAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'tppIgnoreErrors' - If set to true, AWS Service Catalog stops managing the specified ProvisionedProduct object even if it cannot delete the underlying resources.
--
-- * 'tppProvisionedProductId' - The identifier of the ProvisionedProduct object to terminate. Specify either @ProvisionedProductName@ or @ProvisionedProductId@ , but not both.
--
-- * 'tppTerminateToken' - An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the ProvisionedProduct object is terminated, further requests to terminate the same ProvisionedProduct object always return __ResourceNotFound__ regardless of the value of @TerminateToken@ .
terminateProvisionedProduct
    :: Text -- ^ 'tppTerminateToken'
    -> TerminateProvisionedProduct
terminateProvisionedProduct pTerminateToken_ =
    TerminateProvisionedProduct'
    { _tppProvisionedProductName = Nothing
    , _tppAcceptLanguage = Nothing
    , _tppIgnoreErrors = Nothing
    , _tppProvisionedProductId = Nothing
    , _tppTerminateToken = pTerminateToken_
    }

-- | The name of the ProvisionedProduct object to terminate. Specify either @ProvisionedProductName@ or @ProvisionedProductId@ , but not both.
tppProvisionedProductName :: Lens' TerminateProvisionedProduct (Maybe Text)
tppProvisionedProductName = lens _tppProvisionedProductName (\ s a -> s{_tppProvisionedProductName = a});

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
tppAcceptLanguage :: Lens' TerminateProvisionedProduct (Maybe Text)
tppAcceptLanguage = lens _tppAcceptLanguage (\ s a -> s{_tppAcceptLanguage = a});

-- | If set to true, AWS Service Catalog stops managing the specified ProvisionedProduct object even if it cannot delete the underlying resources.
tppIgnoreErrors :: Lens' TerminateProvisionedProduct (Maybe Bool)
tppIgnoreErrors = lens _tppIgnoreErrors (\ s a -> s{_tppIgnoreErrors = a});

-- | The identifier of the ProvisionedProduct object to terminate. Specify either @ProvisionedProductName@ or @ProvisionedProductId@ , but not both.
tppProvisionedProductId :: Lens' TerminateProvisionedProduct (Maybe Text)
tppProvisionedProductId = lens _tppProvisionedProductId (\ s a -> s{_tppProvisionedProductId = a});

-- | An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the ProvisionedProduct object is terminated, further requests to terminate the same ProvisionedProduct object always return __ResourceNotFound__ regardless of the value of @TerminateToken@ .
tppTerminateToken :: Lens' TerminateProvisionedProduct Text
tppTerminateToken = lens _tppTerminateToken (\ s a -> s{_tppTerminateToken = a});

instance AWSRequest TerminateProvisionedProduct where
        type Rs TerminateProvisionedProduct =
             TerminateProvisionedProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 TerminateProvisionedProductResponse' <$>
                   (x .?> "RecordDetail") <*> (pure (fromEnum s)))

instance Hashable TerminateProvisionedProduct

instance NFData TerminateProvisionedProduct

instance ToHeaders TerminateProvisionedProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.TerminateProvisionedProduct"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TerminateProvisionedProduct where
        toJSON TerminateProvisionedProduct'{..}
          = object
              (catMaybes
                 [("ProvisionedProductName" .=) <$>
                    _tppProvisionedProductName,
                  ("AcceptLanguage" .=) <$> _tppAcceptLanguage,
                  ("IgnoreErrors" .=) <$> _tppIgnoreErrors,
                  ("ProvisionedProductId" .=) <$>
                    _tppProvisionedProductId,
                  Just ("TerminateToken" .= _tppTerminateToken)])

instance ToPath TerminateProvisionedProduct where
        toPath = const "/"

instance ToQuery TerminateProvisionedProduct where
        toQuery = const mempty

-- | /See:/ 'terminateProvisionedProductResponse' smart constructor.
data TerminateProvisionedProductResponse = TerminateProvisionedProductResponse'
    { _tpprsRecordDetail   :: !(Maybe RecordDetail)
    , _tpprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpprsRecordDetail' - The detailed result of the 'TerminateProvisionedProduct' request, containing the inputs made to that request, the current state of the request, a pointer to the ProvisionedProduct object that the request is modifying, and a list of any errors that the request encountered.
--
-- * 'tpprsResponseStatus' - -- | The response status code.
terminateProvisionedProductResponse
    :: Int -- ^ 'tpprsResponseStatus'
    -> TerminateProvisionedProductResponse
terminateProvisionedProductResponse pResponseStatus_ =
    TerminateProvisionedProductResponse'
    { _tpprsRecordDetail = Nothing
    , _tpprsResponseStatus = pResponseStatus_
    }

-- | The detailed result of the 'TerminateProvisionedProduct' request, containing the inputs made to that request, the current state of the request, a pointer to the ProvisionedProduct object that the request is modifying, and a list of any errors that the request encountered.
tpprsRecordDetail :: Lens' TerminateProvisionedProductResponse (Maybe RecordDetail)
tpprsRecordDetail = lens _tpprsRecordDetail (\ s a -> s{_tpprsRecordDetail = a});

-- | -- | The response status code.
tpprsResponseStatus :: Lens' TerminateProvisionedProductResponse Int
tpprsResponseStatus = lens _tpprsResponseStatus (\ s a -> s{_tpprsResponseStatus = a});

instance NFData TerminateProvisionedProductResponse
