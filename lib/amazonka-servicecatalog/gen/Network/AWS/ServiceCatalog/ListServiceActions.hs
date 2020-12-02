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
-- Module      : Network.AWS.ServiceCatalog.ListServiceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all self-service actions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActions
  ( -- * Creating a Request
    listServiceActions,
    ListServiceActions,

    -- * Request Lenses
    lsaAcceptLanguage,
    lsaPageToken,
    lsaPageSize,

    -- * Destructuring the Response
    listServiceActionsResponse,
    ListServiceActionsResponse,

    -- * Response Lenses
    lsarsNextPageToken,
    lsarsServiceActionSummaries,
    lsarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listServiceActions' smart constructor.
data ListServiceActions = ListServiceActions'
  { _lsaAcceptLanguage ::
      !(Maybe Text),
    _lsaPageToken :: !(Maybe Text),
    _lsaPageSize :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListServiceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lsaPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lsaPageSize' - The maximum number of items to return with this call.
listServiceActions ::
  ListServiceActions
listServiceActions =
  ListServiceActions'
    { _lsaAcceptLanguage = Nothing,
      _lsaPageToken = Nothing,
      _lsaPageSize = Nothing
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lsaAcceptLanguage :: Lens' ListServiceActions (Maybe Text)
lsaAcceptLanguage = lens _lsaAcceptLanguage (\s a -> s {_lsaAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lsaPageToken :: Lens' ListServiceActions (Maybe Text)
lsaPageToken = lens _lsaPageToken (\s a -> s {_lsaPageToken = a})

-- | The maximum number of items to return with this call.
lsaPageSize :: Lens' ListServiceActions (Maybe Natural)
lsaPageSize = lens _lsaPageSize (\s a -> s {_lsaPageSize = a}) . mapping _Nat

instance AWSPager ListServiceActions where
  page rq rs
    | stop (rs ^. lsarsNextPageToken) = Nothing
    | stop (rs ^. lsarsServiceActionSummaries) = Nothing
    | otherwise = Just $ rq & lsaPageToken .~ rs ^. lsarsNextPageToken

instance AWSRequest ListServiceActions where
  type Rs ListServiceActions = ListServiceActionsResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListServiceActionsResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "ServiceActionSummaries" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListServiceActions

instance NFData ListServiceActions

instance ToHeaders ListServiceActions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.ListServiceActions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListServiceActions where
  toJSON ListServiceActions' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _lsaAcceptLanguage,
            ("PageToken" .=) <$> _lsaPageToken,
            ("PageSize" .=) <$> _lsaPageSize
          ]
      )

instance ToPath ListServiceActions where
  toPath = const "/"

instance ToQuery ListServiceActions where
  toQuery = const mempty

-- | /See:/ 'listServiceActionsResponse' smart constructor.
data ListServiceActionsResponse = ListServiceActionsResponse'
  { _lsarsNextPageToken ::
      !(Maybe Text),
    _lsarsServiceActionSummaries ::
      !(Maybe [ServiceActionSummary]),
    _lsarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListServiceActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsarsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lsarsServiceActionSummaries' - An object containing information about the service actions associated with the provisioning artifact.
--
-- * 'lsarsResponseStatus' - -- | The response status code.
listServiceActionsResponse ::
  -- | 'lsarsResponseStatus'
  Int ->
  ListServiceActionsResponse
listServiceActionsResponse pResponseStatus_ =
  ListServiceActionsResponse'
    { _lsarsNextPageToken = Nothing,
      _lsarsServiceActionSummaries = Nothing,
      _lsarsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lsarsNextPageToken :: Lens' ListServiceActionsResponse (Maybe Text)
lsarsNextPageToken = lens _lsarsNextPageToken (\s a -> s {_lsarsNextPageToken = a})

-- | An object containing information about the service actions associated with the provisioning artifact.
lsarsServiceActionSummaries :: Lens' ListServiceActionsResponse [ServiceActionSummary]
lsarsServiceActionSummaries = lens _lsarsServiceActionSummaries (\s a -> s {_lsarsServiceActionSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lsarsResponseStatus :: Lens' ListServiceActionsResponse Int
lsarsResponseStatus = lens _lsarsResponseStatus (\s a -> s {_lsarsResponseStatus = a})

instance NFData ListServiceActionsResponse
