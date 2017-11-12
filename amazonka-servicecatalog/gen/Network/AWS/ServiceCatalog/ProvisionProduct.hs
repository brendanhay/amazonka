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
-- Module      : Network.AWS.ServiceCatalog.ProvisionProduct
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a /provision/ of a specified product. A /provisioned product/ is a resourced instance for a product. For example, provisioning a CloudFormation-template-backed product results in launching a CloudFormation stack and all the underlying resources that come with it.
--
--
-- You can check the status of this request using the 'DescribeRecord' operation. The error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ " indicates that your request contains a tag which has a tag key but no corresponding tag value (value is empty or null). Your call may have included values returned from a @DescribeProvisioningParameters@ call that resulted in a TagOption key with an empty list. This happens when TagOption keys are in conflict. For more information, see 'DescribeProvisioningParameters' .
--
module Network.AWS.ServiceCatalog.ProvisionProduct
    (
    -- * Creating a Request
      provisionProduct
    , ProvisionProduct
    -- * Request Lenses
    , ppNotificationARNs
    , ppAcceptLanguage
    , ppPathId
    , ppProvisioningParameters
    , ppTags
    , ppProductId
    , ppProvisioningArtifactId
    , ppProvisionedProductName
    , ppProvisionToken

    -- * Destructuring the Response
    , provisionProductResponse
    , ProvisionProductResponse
    -- * Response Lenses
    , pprsRecordDetail
    , pprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'provisionProduct' smart constructor.
data ProvisionProduct = ProvisionProduct'
  { _ppNotificationARNs       :: !(Maybe [Text])
  , _ppAcceptLanguage         :: !(Maybe Text)
  , _ppPathId                 :: !(Maybe Text)
  , _ppProvisioningParameters :: !(Maybe [ProvisioningParameter])
  , _ppTags                   :: !(Maybe [Tag])
  , _ppProductId              :: !Text
  , _ppProvisioningArtifactId :: !Text
  , _ppProvisionedProductName :: !Text
  , _ppProvisionToken         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppNotificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- * 'ppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'ppPathId' - The identifier of the path for this product's provisioning. This value is optional if the product has a default path, and is required if there is more than one path for the specified product.
--
-- * 'ppProvisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
--
-- * 'ppTags' - A list of tags to use as provisioning options.
--
-- * 'ppProductId' - The product identifier.
--
-- * 'ppProvisioningArtifactId' - The provisioning artifact identifier for this product. This is sometimes referred to as the product version.
--
-- * 'ppProvisionedProductName' - A user-friendly name to identify the ProvisionedProduct object. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- * 'ppProvisionToken' - An idempotency token that uniquely identifies the provisioning request.
provisionProduct
    :: Text -- ^ 'ppProductId'
    -> Text -- ^ 'ppProvisioningArtifactId'
    -> Text -- ^ 'ppProvisionedProductName'
    -> Text -- ^ 'ppProvisionToken'
    -> ProvisionProduct
provisionProduct pProductId_ pProvisioningArtifactId_ pProvisionedProductName_ pProvisionToken_ =
  ProvisionProduct'
  { _ppNotificationARNs = Nothing
  , _ppAcceptLanguage = Nothing
  , _ppPathId = Nothing
  , _ppProvisioningParameters = Nothing
  , _ppTags = Nothing
  , _ppProductId = pProductId_
  , _ppProvisioningArtifactId = pProvisioningArtifactId_
  , _ppProvisionedProductName = pProvisionedProductName_
  , _ppProvisionToken = pProvisionToken_
  }


-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
ppNotificationARNs :: Lens' ProvisionProduct [Text]
ppNotificationARNs = lens _ppNotificationARNs (\ s a -> s{_ppNotificationARNs = a}) . _Default . _Coerce;

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
ppAcceptLanguage :: Lens' ProvisionProduct (Maybe Text)
ppAcceptLanguage = lens _ppAcceptLanguage (\ s a -> s{_ppAcceptLanguage = a});

-- | The identifier of the path for this product's provisioning. This value is optional if the product has a default path, and is required if there is more than one path for the specified product.
ppPathId :: Lens' ProvisionProduct (Maybe Text)
ppPathId = lens _ppPathId (\ s a -> s{_ppPathId = a});

-- | Parameters specified by the administrator that are required for provisioning the product.
ppProvisioningParameters :: Lens' ProvisionProduct [ProvisioningParameter]
ppProvisioningParameters = lens _ppProvisioningParameters (\ s a -> s{_ppProvisioningParameters = a}) . _Default . _Coerce;

-- | A list of tags to use as provisioning options.
ppTags :: Lens' ProvisionProduct [Tag]
ppTags = lens _ppTags (\ s a -> s{_ppTags = a}) . _Default . _Coerce;

-- | The product identifier.
ppProductId :: Lens' ProvisionProduct Text
ppProductId = lens _ppProductId (\ s a -> s{_ppProductId = a});

-- | The provisioning artifact identifier for this product. This is sometimes referred to as the product version.
ppProvisioningArtifactId :: Lens' ProvisionProduct Text
ppProvisioningArtifactId = lens _ppProvisioningArtifactId (\ s a -> s{_ppProvisioningArtifactId = a});

-- | A user-friendly name to identify the ProvisionedProduct object. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
ppProvisionedProductName :: Lens' ProvisionProduct Text
ppProvisionedProductName = lens _ppProvisionedProductName (\ s a -> s{_ppProvisionedProductName = a});

-- | An idempotency token that uniquely identifies the provisioning request.
ppProvisionToken :: Lens' ProvisionProduct Text
ppProvisionToken = lens _ppProvisionToken (\ s a -> s{_ppProvisionToken = a});

instance AWSRequest ProvisionProduct where
        type Rs ProvisionProduct = ProvisionProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ProvisionProductResponse' <$>
                   (x .?> "RecordDetail") <*> (pure (fromEnum s)))

instance Hashable ProvisionProduct where

instance NFData ProvisionProduct where

instance ToHeaders ProvisionProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ProvisionProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ProvisionProduct where
        toJSON ProvisionProduct'{..}
          = object
              (catMaybes
                 [("NotificationArns" .=) <$> _ppNotificationARNs,
                  ("AcceptLanguage" .=) <$> _ppAcceptLanguage,
                  ("PathId" .=) <$> _ppPathId,
                  ("ProvisioningParameters" .=) <$>
                    _ppProvisioningParameters,
                  ("Tags" .=) <$> _ppTags,
                  Just ("ProductId" .= _ppProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _ppProvisioningArtifactId),
                  Just
                    ("ProvisionedProductName" .=
                       _ppProvisionedProductName),
                  Just ("ProvisionToken" .= _ppProvisionToken)])

instance ToPath ProvisionProduct where
        toPath = const "/"

instance ToQuery ProvisionProduct where
        toQuery = const mempty

-- | /See:/ 'provisionProductResponse' smart constructor.
data ProvisionProductResponse = ProvisionProductResponse'
  { _pprsRecordDetail   :: !(Maybe RecordDetail)
  , _pprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsRecordDetail' - The detailed result of the 'ProvisionProduct' request, containing the inputs made to that request, the current state of the request, a pointer to the ProvisionedProduct object of the request, and a list of any errors that the request encountered.
--
-- * 'pprsResponseStatus' - -- | The response status code.
provisionProductResponse
    :: Int -- ^ 'pprsResponseStatus'
    -> ProvisionProductResponse
provisionProductResponse pResponseStatus_ =
  ProvisionProductResponse'
  {_pprsRecordDetail = Nothing, _pprsResponseStatus = pResponseStatus_}


-- | The detailed result of the 'ProvisionProduct' request, containing the inputs made to that request, the current state of the request, a pointer to the ProvisionedProduct object of the request, and a list of any errors that the request encountered.
pprsRecordDetail :: Lens' ProvisionProductResponse (Maybe RecordDetail)
pprsRecordDetail = lens _pprsRecordDetail (\ s a -> s{_pprsRecordDetail = a});

-- | -- | The response status code.
pprsResponseStatus :: Lens' ProvisionProductResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\ s a -> s{_pprsResponseStatus = a});

instance NFData ProvisionProductResponse where
