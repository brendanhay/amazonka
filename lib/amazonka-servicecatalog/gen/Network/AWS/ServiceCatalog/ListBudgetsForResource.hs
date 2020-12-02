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
-- Module      : Network.AWS.ServiceCatalog.ListBudgetsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the budgets associated to the specified resource.
module Network.AWS.ServiceCatalog.ListBudgetsForResource
  ( -- * Creating a Request
    listBudgetsForResource,
    ListBudgetsForResource,

    -- * Request Lenses
    lbfrAcceptLanguage,
    lbfrPageToken,
    lbfrPageSize,
    lbfrResourceId,

    -- * Destructuring the Response
    listBudgetsForResourceResponse,
    ListBudgetsForResourceResponse,

    -- * Response Lenses
    lbfrrsNextPageToken,
    lbfrrsBudgets,
    lbfrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listBudgetsForResource' smart constructor.
data ListBudgetsForResource = ListBudgetsForResource'
  { _lbfrAcceptLanguage ::
      !(Maybe Text),
    _lbfrPageToken :: !(Maybe Text),
    _lbfrPageSize :: !(Maybe Nat),
    _lbfrResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBudgetsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbfrAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lbfrPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lbfrPageSize' - The maximum number of items to return with this call.
--
-- * 'lbfrResourceId' - The resource identifier.
listBudgetsForResource ::
  -- | 'lbfrResourceId'
  Text ->
  ListBudgetsForResource
listBudgetsForResource pResourceId_ =
  ListBudgetsForResource'
    { _lbfrAcceptLanguage = Nothing,
      _lbfrPageToken = Nothing,
      _lbfrPageSize = Nothing,
      _lbfrResourceId = pResourceId_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lbfrAcceptLanguage :: Lens' ListBudgetsForResource (Maybe Text)
lbfrAcceptLanguage = lens _lbfrAcceptLanguage (\s a -> s {_lbfrAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lbfrPageToken :: Lens' ListBudgetsForResource (Maybe Text)
lbfrPageToken = lens _lbfrPageToken (\s a -> s {_lbfrPageToken = a})

-- | The maximum number of items to return with this call.
lbfrPageSize :: Lens' ListBudgetsForResource (Maybe Natural)
lbfrPageSize = lens _lbfrPageSize (\s a -> s {_lbfrPageSize = a}) . mapping _Nat

-- | The resource identifier.
lbfrResourceId :: Lens' ListBudgetsForResource Text
lbfrResourceId = lens _lbfrResourceId (\s a -> s {_lbfrResourceId = a})

instance AWSRequest ListBudgetsForResource where
  type Rs ListBudgetsForResource = ListBudgetsForResourceResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListBudgetsForResourceResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "Budgets" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListBudgetsForResource

instance NFData ListBudgetsForResource

instance ToHeaders ListBudgetsForResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.ListBudgetsForResource" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListBudgetsForResource where
  toJSON ListBudgetsForResource' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _lbfrAcceptLanguage,
            ("PageToken" .=) <$> _lbfrPageToken,
            ("PageSize" .=) <$> _lbfrPageSize,
            Just ("ResourceId" .= _lbfrResourceId)
          ]
      )

instance ToPath ListBudgetsForResource where
  toPath = const "/"

instance ToQuery ListBudgetsForResource where
  toQuery = const mempty

-- | /See:/ 'listBudgetsForResourceResponse' smart constructor.
data ListBudgetsForResourceResponse = ListBudgetsForResourceResponse'
  { _lbfrrsNextPageToken ::
      !(Maybe Text),
    _lbfrrsBudgets ::
      !(Maybe [BudgetDetail]),
    _lbfrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBudgetsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbfrrsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lbfrrsBudgets' - Information about the associated budgets.
--
-- * 'lbfrrsResponseStatus' - -- | The response status code.
listBudgetsForResourceResponse ::
  -- | 'lbfrrsResponseStatus'
  Int ->
  ListBudgetsForResourceResponse
listBudgetsForResourceResponse pResponseStatus_ =
  ListBudgetsForResourceResponse'
    { _lbfrrsNextPageToken = Nothing,
      _lbfrrsBudgets = Nothing,
      _lbfrrsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lbfrrsNextPageToken :: Lens' ListBudgetsForResourceResponse (Maybe Text)
lbfrrsNextPageToken = lens _lbfrrsNextPageToken (\s a -> s {_lbfrrsNextPageToken = a})

-- | Information about the associated budgets.
lbfrrsBudgets :: Lens' ListBudgetsForResourceResponse [BudgetDetail]
lbfrrsBudgets = lens _lbfrrsBudgets (\s a -> s {_lbfrrsBudgets = a}) . _Default . _Coerce

-- | -- | The response status code.
lbfrrsResponseStatus :: Lens' ListBudgetsForResourceResponse Int
lbfrrsResponseStatus = lens _lbfrrsResponseStatus (\s a -> s {_lbfrrsResponseStatus = a})

instance NFData ListBudgetsForResourceResponse
