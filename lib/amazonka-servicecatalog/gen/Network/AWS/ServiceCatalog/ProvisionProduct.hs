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
-- Module      : Network.AWS.ServiceCatalog.ProvisionProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions the specified product.
--
--
-- A provisioned product is a resourced instance of a product. For example, provisioning a product based on a CloudFormation template launches a CloudFormation stack and its underlying resources. You can check the status of this request using 'DescribeRecord' .
--
-- If the request contains a tag key with an empty list of values, there is a tag conflict for that key. Do not include conflicted keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ".
module Network.AWS.ServiceCatalog.ProvisionProduct
  ( -- * Creating a Request
    provisionProduct,
    ProvisionProduct,

    -- * Request Lenses
    ppProductName,
    ppProvisioningArtifactId,
    ppProvisioningArtifactName,
    ppNotificationARNs,
    ppPathName,
    ppAcceptLanguage,
    ppPathId,
    ppProvisioningParameters,
    ppProductId,
    ppTags,
    ppProvisioningPreferences,
    ppProvisionedProductName,
    ppProvisionToken,

    -- * Destructuring the Response
    provisionProductResponse,
    ProvisionProductResponse,

    -- * Response Lenses
    pprsRecordDetail,
    pprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'provisionProduct' smart constructor.
data ProvisionProduct = ProvisionProduct'
  { _ppProductName ::
      !(Maybe Text),
    _ppProvisioningArtifactId :: !(Maybe Text),
    _ppProvisioningArtifactName :: !(Maybe Text),
    _ppNotificationARNs :: !(Maybe [Text]),
    _ppPathName :: !(Maybe Text),
    _ppAcceptLanguage :: !(Maybe Text),
    _ppPathId :: !(Maybe Text),
    _ppProvisioningParameters ::
      !(Maybe [ProvisioningParameter]),
    _ppProductId :: !(Maybe Text),
    _ppTags :: !(Maybe [Tag]),
    _ppProvisioningPreferences ::
      !(Maybe ProvisioningPreferences),
    _ppProvisionedProductName :: !Text,
    _ppProvisionToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppProductName' - The name of the product. You must provide the name or ID, but not both.
--
-- * 'ppProvisioningArtifactId' - The identifier of the provisioning artifact. You must provide the name or ID, but not both.
--
-- * 'ppProvisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- * 'ppNotificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- * 'ppPathName' - The name of the path. You must provide the name or ID, but not both.
--
-- * 'ppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'ppPathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
--
-- * 'ppProvisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
--
-- * 'ppProductId' - The product identifier. You must provide the name or ID, but not both.
--
-- * 'ppTags' - One or more tags.
--
-- * 'ppProvisioningPreferences' - An object that contains information about the provisioning preferences for a stack set.
--
-- * 'ppProvisionedProductName' - A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- * 'ppProvisionToken' - An idempotency token that uniquely identifies the provisioning request.
provisionProduct ::
  -- | 'ppProvisionedProductName'
  Text ->
  -- | 'ppProvisionToken'
  Text ->
  ProvisionProduct
provisionProduct pProvisionedProductName_ pProvisionToken_ =
  ProvisionProduct'
    { _ppProductName = Nothing,
      _ppProvisioningArtifactId = Nothing,
      _ppProvisioningArtifactName = Nothing,
      _ppNotificationARNs = Nothing,
      _ppPathName = Nothing,
      _ppAcceptLanguage = Nothing,
      _ppPathId = Nothing,
      _ppProvisioningParameters = Nothing,
      _ppProductId = Nothing,
      _ppTags = Nothing,
      _ppProvisioningPreferences = Nothing,
      _ppProvisionedProductName = pProvisionedProductName_,
      _ppProvisionToken = pProvisionToken_
    }

-- | The name of the product. You must provide the name or ID, but not both.
ppProductName :: Lens' ProvisionProduct (Maybe Text)
ppProductName = lens _ppProductName (\s a -> s {_ppProductName = a})

-- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
ppProvisioningArtifactId :: Lens' ProvisionProduct (Maybe Text)
ppProvisioningArtifactId = lens _ppProvisioningArtifactId (\s a -> s {_ppProvisioningArtifactId = a})

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
ppProvisioningArtifactName :: Lens' ProvisionProduct (Maybe Text)
ppProvisioningArtifactName = lens _ppProvisioningArtifactName (\s a -> s {_ppProvisioningArtifactName = a})

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
ppNotificationARNs :: Lens' ProvisionProduct [Text]
ppNotificationARNs = lens _ppNotificationARNs (\s a -> s {_ppNotificationARNs = a}) . _Default . _Coerce

-- | The name of the path. You must provide the name or ID, but not both.
ppPathName :: Lens' ProvisionProduct (Maybe Text)
ppPathName = lens _ppPathName (\s a -> s {_ppPathName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
ppAcceptLanguage :: Lens' ProvisionProduct (Maybe Text)
ppAcceptLanguage = lens _ppAcceptLanguage (\s a -> s {_ppAcceptLanguage = a})

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
ppPathId :: Lens' ProvisionProduct (Maybe Text)
ppPathId = lens _ppPathId (\s a -> s {_ppPathId = a})

-- | Parameters specified by the administrator that are required for provisioning the product.
ppProvisioningParameters :: Lens' ProvisionProduct [ProvisioningParameter]
ppProvisioningParameters = lens _ppProvisioningParameters (\s a -> s {_ppProvisioningParameters = a}) . _Default . _Coerce

-- | The product identifier. You must provide the name or ID, but not both.
ppProductId :: Lens' ProvisionProduct (Maybe Text)
ppProductId = lens _ppProductId (\s a -> s {_ppProductId = a})

-- | One or more tags.
ppTags :: Lens' ProvisionProduct [Tag]
ppTags = lens _ppTags (\s a -> s {_ppTags = a}) . _Default . _Coerce

-- | An object that contains information about the provisioning preferences for a stack set.
ppProvisioningPreferences :: Lens' ProvisionProduct (Maybe ProvisioningPreferences)
ppProvisioningPreferences = lens _ppProvisioningPreferences (\s a -> s {_ppProvisioningPreferences = a})

-- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
ppProvisionedProductName :: Lens' ProvisionProduct Text
ppProvisionedProductName = lens _ppProvisionedProductName (\s a -> s {_ppProvisionedProductName = a})

-- | An idempotency token that uniquely identifies the provisioning request.
ppProvisionToken :: Lens' ProvisionProduct Text
ppProvisionToken = lens _ppProvisionToken (\s a -> s {_ppProvisionToken = a})

instance AWSRequest ProvisionProduct where
  type Rs ProvisionProduct = ProvisionProductResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ProvisionProductResponse'
            <$> (x .?> "RecordDetail") <*> (pure (fromEnum s))
      )

instance Hashable ProvisionProduct

instance NFData ProvisionProduct

instance ToHeaders ProvisionProduct where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.ProvisionProduct" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ProvisionProduct where
  toJSON ProvisionProduct' {..} =
    object
      ( catMaybes
          [ ("ProductName" .=) <$> _ppProductName,
            ("ProvisioningArtifactId" .=) <$> _ppProvisioningArtifactId,
            ("ProvisioningArtifactName" .=) <$> _ppProvisioningArtifactName,
            ("NotificationArns" .=) <$> _ppNotificationARNs,
            ("PathName" .=) <$> _ppPathName,
            ("AcceptLanguage" .=) <$> _ppAcceptLanguage,
            ("PathId" .=) <$> _ppPathId,
            ("ProvisioningParameters" .=) <$> _ppProvisioningParameters,
            ("ProductId" .=) <$> _ppProductId,
            ("Tags" .=) <$> _ppTags,
            ("ProvisioningPreferences" .=) <$> _ppProvisioningPreferences,
            Just ("ProvisionedProductName" .= _ppProvisionedProductName),
            Just ("ProvisionToken" .= _ppProvisionToken)
          ]
      )

instance ToPath ProvisionProduct where
  toPath = const "/"

instance ToQuery ProvisionProduct where
  toQuery = const mempty

-- | /See:/ 'provisionProductResponse' smart constructor.
data ProvisionProductResponse = ProvisionProductResponse'
  { _pprsRecordDetail ::
      !(Maybe RecordDetail),
    _pprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsRecordDetail' - Information about the result of provisioning the product.
--
-- * 'pprsResponseStatus' - -- | The response status code.
provisionProductResponse ::
  -- | 'pprsResponseStatus'
  Int ->
  ProvisionProductResponse
provisionProductResponse pResponseStatus_ =
  ProvisionProductResponse'
    { _pprsRecordDetail = Nothing,
      _pprsResponseStatus = pResponseStatus_
    }

-- | Information about the result of provisioning the product.
pprsRecordDetail :: Lens' ProvisionProductResponse (Maybe RecordDetail)
pprsRecordDetail = lens _pprsRecordDetail (\s a -> s {_pprsRecordDetail = a})

-- | -- | The response status code.
pprsResponseStatus :: Lens' ProvisionProductResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\s a -> s {_pprsResponseStatus = a})

instance NFData ProvisionProductResponse
