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
-- Module      : Network.AWS.ServiceCatalog.ListPortfolioAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account IDs that have access to the specified portfolio.
--
--
-- A delegated admin can list the accounts that have access to the shared portfolio. Note that if a delegated admin is de-registered, they can no longer perform this operation.
module Network.AWS.ServiceCatalog.ListPortfolioAccess
  ( -- * Creating a Request
    listPortfolioAccess,
    ListPortfolioAccess,

    -- * Request Lenses
    lOrganizationParentId,
    lAcceptLanguage,
    lPageToken,
    lPageSize,
    lPortfolioId,

    -- * Destructuring the Response
    listPortfolioAccessResponse,
    ListPortfolioAccessResponse,

    -- * Response Lenses
    lparsNextPageToken,
    lparsAccountIds,
    lparsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { _lOrganizationParentId ::
      !(Maybe Text),
    _lAcceptLanguage :: !(Maybe Text),
    _lPageToken :: !(Maybe Text),
    _lPageSize :: !(Maybe Nat),
    _lPortfolioId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPortfolioAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lOrganizationParentId' - The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
--
-- * 'lAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lPageSize' - The maximum number of items to return with this call.
--
-- * 'lPortfolioId' - The portfolio identifier.
listPortfolioAccess ::
  -- | 'lPortfolioId'
  Text ->
  ListPortfolioAccess
listPortfolioAccess pPortfolioId_ =
  ListPortfolioAccess'
    { _lOrganizationParentId = Nothing,
      _lAcceptLanguage = Nothing,
      _lPageToken = Nothing,
      _lPageSize = Nothing,
      _lPortfolioId = pPortfolioId_
    }

-- | The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
lOrganizationParentId :: Lens' ListPortfolioAccess (Maybe Text)
lOrganizationParentId = lens _lOrganizationParentId (\s a -> s {_lOrganizationParentId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lAcceptLanguage :: Lens' ListPortfolioAccess (Maybe Text)
lAcceptLanguage = lens _lAcceptLanguage (\s a -> s {_lAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lPageToken :: Lens' ListPortfolioAccess (Maybe Text)
lPageToken = lens _lPageToken (\s a -> s {_lPageToken = a})

-- | The maximum number of items to return with this call.
lPageSize :: Lens' ListPortfolioAccess (Maybe Natural)
lPageSize = lens _lPageSize (\s a -> s {_lPageSize = a}) . mapping _Nat

-- | The portfolio identifier.
lPortfolioId :: Lens' ListPortfolioAccess Text
lPortfolioId = lens _lPortfolioId (\s a -> s {_lPortfolioId = a})

instance AWSRequest ListPortfolioAccess where
  type Rs ListPortfolioAccess = ListPortfolioAccessResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListPortfolioAccessResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "AccountIds" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListPortfolioAccess

instance NFData ListPortfolioAccess

instance ToHeaders ListPortfolioAccess where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.ListPortfolioAccess" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListPortfolioAccess where
  toJSON ListPortfolioAccess' {..} =
    object
      ( catMaybes
          [ ("OrganizationParentId" .=) <$> _lOrganizationParentId,
            ("AcceptLanguage" .=) <$> _lAcceptLanguage,
            ("PageToken" .=) <$> _lPageToken,
            ("PageSize" .=) <$> _lPageSize,
            Just ("PortfolioId" .= _lPortfolioId)
          ]
      )

instance ToPath ListPortfolioAccess where
  toPath = const "/"

instance ToQuery ListPortfolioAccess where
  toQuery = const mempty

-- | /See:/ 'listPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { _lparsNextPageToken ::
      !(Maybe Text),
    _lparsAccountIds :: !(Maybe [Text]),
    _lparsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPortfolioAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lparsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lparsAccountIds' - Information about the AWS accounts with access to the portfolio.
--
-- * 'lparsResponseStatus' - -- | The response status code.
listPortfolioAccessResponse ::
  -- | 'lparsResponseStatus'
  Int ->
  ListPortfolioAccessResponse
listPortfolioAccessResponse pResponseStatus_ =
  ListPortfolioAccessResponse'
    { _lparsNextPageToken = Nothing,
      _lparsAccountIds = Nothing,
      _lparsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lparsNextPageToken :: Lens' ListPortfolioAccessResponse (Maybe Text)
lparsNextPageToken = lens _lparsNextPageToken (\s a -> s {_lparsNextPageToken = a})

-- | Information about the AWS accounts with access to the portfolio.
lparsAccountIds :: Lens' ListPortfolioAccessResponse [Text]
lparsAccountIds = lens _lparsAccountIds (\s a -> s {_lparsAccountIds = a}) . _Default . _Coerce

-- | -- | The response status code.
lparsResponseStatus :: Lens' ListPortfolioAccessResponse Int
lparsResponseStatus = lens _lparsResponseStatus (\s a -> s {_lparsResponseStatus = a})

instance NFData ListPortfolioAccessResponse
