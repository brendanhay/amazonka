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
-- Module      : Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API takes either a @ProvisonedProductId@ or a @ProvisionedProductName@ , along with a list of one or more output keys, and responds with the key/value pairs of those outputs.
module Network.AWS.ServiceCatalog.GetProvisionedProductOutputs
  ( -- * Creating a Request
    getProvisionedProductOutputs,
    GetProvisionedProductOutputs,

    -- * Request Lenses
    gppoProvisionedProductName,
    gppoOutputKeys,
    gppoAcceptLanguage,
    gppoPageToken,
    gppoPageSize,
    gppoProvisionedProductId,

    -- * Destructuring the Response
    getProvisionedProductOutputsResponse,
    GetProvisionedProductOutputsResponse,

    -- * Response Lenses
    gpporsNextPageToken,
    gpporsOutputs,
    gpporsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'getProvisionedProductOutputs' smart constructor.
data GetProvisionedProductOutputs = GetProvisionedProductOutputs'
  { _gppoProvisionedProductName ::
      !(Maybe Text),
    _gppoOutputKeys ::
      !(Maybe [Text]),
    _gppoAcceptLanguage ::
      !(Maybe Text),
    _gppoPageToken :: !(Maybe Text),
    _gppoPageSize :: !(Maybe Nat),
    _gppoProvisionedProductId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProvisionedProductOutputs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gppoProvisionedProductName' - The name of the provisioned product that you want the outputs from.
--
-- * 'gppoOutputKeys' - The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
--
-- * 'gppoAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'gppoPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'gppoPageSize' - The maximum number of items to return with this call.
--
-- * 'gppoProvisionedProductId' - The identifier of the provisioned product that you want the outputs from.
getProvisionedProductOutputs ::
  GetProvisionedProductOutputs
getProvisionedProductOutputs =
  GetProvisionedProductOutputs'
    { _gppoProvisionedProductName =
        Nothing,
      _gppoOutputKeys = Nothing,
      _gppoAcceptLanguage = Nothing,
      _gppoPageToken = Nothing,
      _gppoPageSize = Nothing,
      _gppoProvisionedProductId = Nothing
    }

-- | The name of the provisioned product that you want the outputs from.
gppoProvisionedProductName :: Lens' GetProvisionedProductOutputs (Maybe Text)
gppoProvisionedProductName = lens _gppoProvisionedProductName (\s a -> s {_gppoProvisionedProductName = a})

-- | The list of keys that the API should return with their values. If none are provided, the API will return all outputs of the provisioned product.
gppoOutputKeys :: Lens' GetProvisionedProductOutputs [Text]
gppoOutputKeys = lens _gppoOutputKeys (\s a -> s {_gppoOutputKeys = a}) . _Default . _Coerce

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
gppoAcceptLanguage :: Lens' GetProvisionedProductOutputs (Maybe Text)
gppoAcceptLanguage = lens _gppoAcceptLanguage (\s a -> s {_gppoAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
gppoPageToken :: Lens' GetProvisionedProductOutputs (Maybe Text)
gppoPageToken = lens _gppoPageToken (\s a -> s {_gppoPageToken = a})

-- | The maximum number of items to return with this call.
gppoPageSize :: Lens' GetProvisionedProductOutputs (Maybe Natural)
gppoPageSize = lens _gppoPageSize (\s a -> s {_gppoPageSize = a}) . mapping _Nat

-- | The identifier of the provisioned product that you want the outputs from.
gppoProvisionedProductId :: Lens' GetProvisionedProductOutputs (Maybe Text)
gppoProvisionedProductId = lens _gppoProvisionedProductId (\s a -> s {_gppoProvisionedProductId = a})

instance AWSRequest GetProvisionedProductOutputs where
  type
    Rs GetProvisionedProductOutputs =
      GetProvisionedProductOutputsResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          GetProvisionedProductOutputsResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "Outputs" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetProvisionedProductOutputs

instance NFData GetProvisionedProductOutputs

instance ToHeaders GetProvisionedProductOutputs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.GetProvisionedProductOutputs" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetProvisionedProductOutputs where
  toJSON GetProvisionedProductOutputs' {..} =
    object
      ( catMaybes
          [ ("ProvisionedProductName" .=) <$> _gppoProvisionedProductName,
            ("OutputKeys" .=) <$> _gppoOutputKeys,
            ("AcceptLanguage" .=) <$> _gppoAcceptLanguage,
            ("PageToken" .=) <$> _gppoPageToken,
            ("PageSize" .=) <$> _gppoPageSize,
            ("ProvisionedProductId" .=) <$> _gppoProvisionedProductId
          ]
      )

instance ToPath GetProvisionedProductOutputs where
  toPath = const "/"

instance ToQuery GetProvisionedProductOutputs where
  toQuery = const mempty

-- | /See:/ 'getProvisionedProductOutputsResponse' smart constructor.
data GetProvisionedProductOutputsResponse = GetProvisionedProductOutputsResponse'
  { _gpporsNextPageToken ::
      !(Maybe Text),
    _gpporsOutputs ::
      !( Maybe
           [RecordOutput]
       ),
    _gpporsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProvisionedProductOutputsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpporsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'gpporsOutputs' - Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- * 'gpporsResponseStatus' - -- | The response status code.
getProvisionedProductOutputsResponse ::
  -- | 'gpporsResponseStatus'
  Int ->
  GetProvisionedProductOutputsResponse
getProvisionedProductOutputsResponse pResponseStatus_ =
  GetProvisionedProductOutputsResponse'
    { _gpporsNextPageToken =
        Nothing,
      _gpporsOutputs = Nothing,
      _gpporsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
gpporsNextPageToken :: Lens' GetProvisionedProductOutputsResponse (Maybe Text)
gpporsNextPageToken = lens _gpporsNextPageToken (\s a -> s {_gpporsNextPageToken = a})

-- | Information about the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
gpporsOutputs :: Lens' GetProvisionedProductOutputsResponse [RecordOutput]
gpporsOutputs = lens _gpporsOutputs (\s a -> s {_gpporsOutputs = a}) . _Default . _Coerce

-- | -- | The response status code.
gpporsResponseStatus :: Lens' GetProvisionedProductOutputsResponse Int
gpporsResponseStatus = lens _gpporsResponseStatus (\s a -> s {_gpporsResponseStatus = a})

instance NFData GetProvisionedProductOutputsResponse
