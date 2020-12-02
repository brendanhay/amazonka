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
-- Module      : Network.AWS.Glue.ListRegistries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registries that you have created, with minimal registry information. Registries in the @Deleting@ status will not be included in the results. Empty results will be returned if there are no registries available.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListRegistries
  ( -- * Creating a Request
    listRegistries,
    ListRegistries,

    -- * Request Lenses
    lrNextToken,
    lrMaxResults,

    -- * Destructuring the Response
    listRegistriesResponse,
    ListRegistriesResponse,

    -- * Response Lenses
    lrrsRegistries,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRegistries' smart constructor.
data ListRegistries = ListRegistries'
  { _lrNextToken ::
      !(Maybe Text),
    _lrMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRegistries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - A continuation token, if this is a continuation call.
--
-- * 'lrMaxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
listRegistries ::
  ListRegistries
listRegistries =
  ListRegistries' {_lrNextToken = Nothing, _lrMaxResults = Nothing}

-- | A continuation token, if this is a continuation call.
lrNextToken :: Lens' ListRegistries (Maybe Text)
lrNextToken = lens _lrNextToken (\s a -> s {_lrNextToken = a})

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
lrMaxResults :: Lens' ListRegistries (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\s a -> s {_lrMaxResults = a}) . mapping _Nat

instance AWSPager ListRegistries where
  page rq rs
    | stop (rs ^. lrrsNextToken) = Nothing
    | stop (rs ^. lrrsRegistries) = Nothing
    | otherwise = Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRegistries where
  type Rs ListRegistries = ListRegistriesResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListRegistriesResponse'
            <$> (x .?> "Registries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListRegistries

instance NFData ListRegistries

instance ToHeaders ListRegistries where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListRegistries" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListRegistries where
  toJSON ListRegistries' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lrNextToken,
            ("MaxResults" .=) <$> _lrMaxResults
          ]
      )

instance ToPath ListRegistries where
  toPath = const "/"

instance ToQuery ListRegistries where
  toQuery = const mempty

-- | /See:/ 'listRegistriesResponse' smart constructor.
data ListRegistriesResponse = ListRegistriesResponse'
  { _lrrsRegistries ::
      !(Maybe [RegistryListItem]),
    _lrrsNextToken :: !(Maybe Text),
    _lrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRegistriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRegistries' - An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
--
-- * 'lrrsNextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRegistriesResponse ::
  -- | 'lrrsResponseStatus'
  Int ->
  ListRegistriesResponse
listRegistriesResponse pResponseStatus_ =
  ListRegistriesResponse'
    { _lrrsRegistries = Nothing,
      _lrrsNextToken = Nothing,
      _lrrsResponseStatus = pResponseStatus_
    }

-- | An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
lrrsRegistries :: Lens' ListRegistriesResponse [RegistryListItem]
lrrsRegistries = lens _lrrsRegistries (\s a -> s {_lrrsRegistries = a}) . _Default . _Coerce

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
lrrsNextToken :: Lens' ListRegistriesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\s a -> s {_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRegistriesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\s a -> s {_lrrsResponseStatus = a})

instance NFData ListRegistriesResponse
