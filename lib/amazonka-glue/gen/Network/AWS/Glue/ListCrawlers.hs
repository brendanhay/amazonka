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
-- Module      : Network.AWS.Glue.ListCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all crawler resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListCrawlers
  ( -- * Creating a Request
    listCrawlers,
    ListCrawlers,

    -- * Request Lenses
    lcNextToken,
    lcMaxResults,
    lcTags,

    -- * Destructuring the Response
    listCrawlersResponse,
    ListCrawlersResponse,

    -- * Response Lenses
    lcrsNextToken,
    lcrsCrawlerNames,
    lcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCrawlers' smart constructor.
data ListCrawlers = ListCrawlers'
  { _lcNextToken :: !(Maybe Text),
    _lcMaxResults :: !(Maybe Nat),
    _lcTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCrawlers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - A continuation token, if this is a continuation request.
--
-- * 'lcMaxResults' - The maximum size of a list to return.
--
-- * 'lcTags' - Specifies to return only these tagged resources.
listCrawlers ::
  ListCrawlers
listCrawlers =
  ListCrawlers'
    { _lcNextToken = Nothing,
      _lcMaxResults = Nothing,
      _lcTags = Nothing
    }

-- | A continuation token, if this is a continuation request.
lcNextToken :: Lens' ListCrawlers (Maybe Text)
lcNextToken = lens _lcNextToken (\s a -> s {_lcNextToken = a})

-- | The maximum size of a list to return.
lcMaxResults :: Lens' ListCrawlers (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\s a -> s {_lcMaxResults = a}) . mapping _Nat

-- | Specifies to return only these tagged resources.
lcTags :: Lens' ListCrawlers (HashMap Text (Text))
lcTags = lens _lcTags (\s a -> s {_lcTags = a}) . _Default . _Map

instance AWSRequest ListCrawlers where
  type Rs ListCrawlers = ListCrawlersResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListCrawlersResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "CrawlerNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListCrawlers

instance NFData ListCrawlers

instance ToHeaders ListCrawlers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListCrawlers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListCrawlers where
  toJSON ListCrawlers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lcNextToken,
            ("MaxResults" .=) <$> _lcMaxResults,
            ("Tags" .=) <$> _lcTags
          ]
      )

instance ToPath ListCrawlers where
  toPath = const "/"

instance ToQuery ListCrawlers where
  toQuery = const mempty

-- | /See:/ 'listCrawlersResponse' smart constructor.
data ListCrawlersResponse = ListCrawlersResponse'
  { _lcrsNextToken ::
      !(Maybe Text),
    _lcrsCrawlerNames :: !(Maybe [Text]),
    _lcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCrawlersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsNextToken' - A continuation token, if the returned list does not contain the last metric available.
--
-- * 'lcrsCrawlerNames' - The names of all crawlers in the account, or the crawlers with the specified tags.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCrawlersResponse ::
  -- | 'lcrsResponseStatus'
  Int ->
  ListCrawlersResponse
listCrawlersResponse pResponseStatus_ =
  ListCrawlersResponse'
    { _lcrsNextToken = Nothing,
      _lcrsCrawlerNames = Nothing,
      _lcrsResponseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list does not contain the last metric available.
lcrsNextToken :: Lens' ListCrawlersResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\s a -> s {_lcrsNextToken = a})

-- | The names of all crawlers in the account, or the crawlers with the specified tags.
lcrsCrawlerNames :: Lens' ListCrawlersResponse [Text]
lcrsCrawlerNames = lens _lcrsCrawlerNames (\s a -> s {_lcrsCrawlerNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCrawlersResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\s a -> s {_lcrsResponseStatus = a})

instance NFData ListCrawlersResponse
