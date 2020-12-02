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
-- Module      : Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of self-service actions associated with the specified Product ID and Provisioning Artifact ID.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
  ( -- * Creating a Request
    listServiceActionsForProvisioningArtifact,
    ListServiceActionsForProvisioningArtifact,

    -- * Request Lenses
    lsafpaAcceptLanguage,
    lsafpaPageToken,
    lsafpaPageSize,
    lsafpaProductId,
    lsafpaProvisioningArtifactId,

    -- * Destructuring the Response
    listServiceActionsForProvisioningArtifactResponse,
    ListServiceActionsForProvisioningArtifactResponse,

    -- * Response Lenses
    lsafparsNextPageToken,
    lsafparsServiceActionSummaries,
    lsafparsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listServiceActionsForProvisioningArtifact' smart constructor.
data ListServiceActionsForProvisioningArtifact = ListServiceActionsForProvisioningArtifact'
  { _lsafpaAcceptLanguage ::
      !( Maybe
           Text
       ),
    _lsafpaPageToken ::
      !( Maybe
           Text
       ),
    _lsafpaPageSize ::
      !( Maybe
           Nat
       ),
    _lsafpaProductId ::
      !Text,
    _lsafpaProvisioningArtifactId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListServiceActionsForProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsafpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lsafpaPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lsafpaPageSize' - The maximum number of items to return with this call.
--
-- * 'lsafpaProductId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- * 'lsafpaProvisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
listServiceActionsForProvisioningArtifact ::
  -- | 'lsafpaProductId'
  Text ->
  -- | 'lsafpaProvisioningArtifactId'
  Text ->
  ListServiceActionsForProvisioningArtifact
listServiceActionsForProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_ =
    ListServiceActionsForProvisioningArtifact'
      { _lsafpaAcceptLanguage =
          Nothing,
        _lsafpaPageToken = Nothing,
        _lsafpaPageSize = Nothing,
        _lsafpaProductId = pProductId_,
        _lsafpaProvisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lsafpaAcceptLanguage :: Lens' ListServiceActionsForProvisioningArtifact (Maybe Text)
lsafpaAcceptLanguage = lens _lsafpaAcceptLanguage (\s a -> s {_lsafpaAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lsafpaPageToken :: Lens' ListServiceActionsForProvisioningArtifact (Maybe Text)
lsafpaPageToken = lens _lsafpaPageToken (\s a -> s {_lsafpaPageToken = a})

-- | The maximum number of items to return with this call.
lsafpaPageSize :: Lens' ListServiceActionsForProvisioningArtifact (Maybe Natural)
lsafpaPageSize = lens _lsafpaPageSize (\s a -> s {_lsafpaPageSize = a}) . mapping _Nat

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
lsafpaProductId :: Lens' ListServiceActionsForProvisioningArtifact Text
lsafpaProductId = lens _lsafpaProductId (\s a -> s {_lsafpaProductId = a})

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
lsafpaProvisioningArtifactId :: Lens' ListServiceActionsForProvisioningArtifact Text
lsafpaProvisioningArtifactId = lens _lsafpaProvisioningArtifactId (\s a -> s {_lsafpaProvisioningArtifactId = a})

instance AWSPager ListServiceActionsForProvisioningArtifact where
  page rq rs
    | stop (rs ^. lsafparsNextPageToken) = Nothing
    | stop (rs ^. lsafparsServiceActionSummaries) = Nothing
    | otherwise =
      Just $ rq & lsafpaPageToken .~ rs ^. lsafparsNextPageToken

instance AWSRequest ListServiceActionsForProvisioningArtifact where
  type
    Rs ListServiceActionsForProvisioningArtifact =
      ListServiceActionsForProvisioningArtifactResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListServiceActionsForProvisioningArtifactResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "ServiceActionSummaries" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListServiceActionsForProvisioningArtifact

instance NFData ListServiceActionsForProvisioningArtifact

instance ToHeaders ListServiceActionsForProvisioningArtifact where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.ListServiceActionsForProvisioningArtifact" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListServiceActionsForProvisioningArtifact where
  toJSON ListServiceActionsForProvisioningArtifact' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _lsafpaAcceptLanguage,
            ("PageToken" .=) <$> _lsafpaPageToken,
            ("PageSize" .=) <$> _lsafpaPageSize,
            Just ("ProductId" .= _lsafpaProductId),
            Just ("ProvisioningArtifactId" .= _lsafpaProvisioningArtifactId)
          ]
      )

instance ToPath ListServiceActionsForProvisioningArtifact where
  toPath = const "/"

instance ToQuery ListServiceActionsForProvisioningArtifact where
  toQuery = const mempty

-- | /See:/ 'listServiceActionsForProvisioningArtifactResponse' smart constructor.
data ListServiceActionsForProvisioningArtifactResponse = ListServiceActionsForProvisioningArtifactResponse'
  { _lsafparsNextPageToken ::
      !( Maybe
           Text
       ),
    _lsafparsServiceActionSummaries ::
      !( Maybe
           [ServiceActionSummary]
       ),
    _lsafparsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListServiceActionsForProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsafparsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lsafparsServiceActionSummaries' - An object containing information about the self-service actions associated with the provisioning artifact.
--
-- * 'lsafparsResponseStatus' - -- | The response status code.
listServiceActionsForProvisioningArtifactResponse ::
  -- | 'lsafparsResponseStatus'
  Int ->
  ListServiceActionsForProvisioningArtifactResponse
listServiceActionsForProvisioningArtifactResponse pResponseStatus_ =
  ListServiceActionsForProvisioningArtifactResponse'
    { _lsafparsNextPageToken =
        Nothing,
      _lsafparsServiceActionSummaries = Nothing,
      _lsafparsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lsafparsNextPageToken :: Lens' ListServiceActionsForProvisioningArtifactResponse (Maybe Text)
lsafparsNextPageToken = lens _lsafparsNextPageToken (\s a -> s {_lsafparsNextPageToken = a})

-- | An object containing information about the self-service actions associated with the provisioning artifact.
lsafparsServiceActionSummaries :: Lens' ListServiceActionsForProvisioningArtifactResponse [ServiceActionSummary]
lsafparsServiceActionSummaries = lens _lsafparsServiceActionSummaries (\s a -> s {_lsafparsServiceActionSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lsafparsResponseStatus :: Lens' ListServiceActionsForProvisioningArtifactResponse Int
lsafparsResponseStatus = lens _lsafparsResponseStatus (\s a -> s {_lsafparsResponseStatus = a})

instance NFData ListServiceActionsForProvisioningArtifactResponse
