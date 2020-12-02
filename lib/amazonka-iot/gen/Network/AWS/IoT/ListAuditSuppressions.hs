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
-- Module      : Network.AWS.IoT.ListAuditSuppressions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender audit listings.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditSuppressions
  ( -- * Creating a Request
    listAuditSuppressions,
    ListAuditSuppressions,

    -- * Request Lenses
    lasCheckName,
    lasNextToken,
    lasAscendingOrder,
    lasMaxResults,
    lasResourceIdentifier,

    -- * Destructuring the Response
    listAuditSuppressionsResponse,
    ListAuditSuppressionsResponse,

    -- * Response Lenses
    lasrsNextToken,
    lasrsSuppressions,
    lasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuditSuppressions' smart constructor.
data ListAuditSuppressions = ListAuditSuppressions'
  { _lasCheckName ::
      !(Maybe Text),
    _lasNextToken :: !(Maybe Text),
    _lasAscendingOrder :: !(Maybe Bool),
    _lasMaxResults :: !(Maybe Nat),
    _lasResourceIdentifier ::
      !(Maybe ResourceIdentifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditSuppressions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasCheckName' - Undocumented member.
--
-- * 'lasNextToken' - The token for the next set of results.
--
-- * 'lasAscendingOrder' - Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ .
--
-- * 'lasMaxResults' - The maximum number of results to return at one time. The default is 25.
--
-- * 'lasResourceIdentifier' - Undocumented member.
listAuditSuppressions ::
  ListAuditSuppressions
listAuditSuppressions =
  ListAuditSuppressions'
    { _lasCheckName = Nothing,
      _lasNextToken = Nothing,
      _lasAscendingOrder = Nothing,
      _lasMaxResults = Nothing,
      _lasResourceIdentifier = Nothing
    }

-- | Undocumented member.
lasCheckName :: Lens' ListAuditSuppressions (Maybe Text)
lasCheckName = lens _lasCheckName (\s a -> s {_lasCheckName = a})

-- | The token for the next set of results.
lasNextToken :: Lens' ListAuditSuppressions (Maybe Text)
lasNextToken = lens _lasNextToken (\s a -> s {_lasNextToken = a})

-- | Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ .
lasAscendingOrder :: Lens' ListAuditSuppressions (Maybe Bool)
lasAscendingOrder = lens _lasAscendingOrder (\s a -> s {_lasAscendingOrder = a})

-- | The maximum number of results to return at one time. The default is 25.
lasMaxResults :: Lens' ListAuditSuppressions (Maybe Natural)
lasMaxResults = lens _lasMaxResults (\s a -> s {_lasMaxResults = a}) . mapping _Nat

-- | Undocumented member.
lasResourceIdentifier :: Lens' ListAuditSuppressions (Maybe ResourceIdentifier)
lasResourceIdentifier = lens _lasResourceIdentifier (\s a -> s {_lasResourceIdentifier = a})

instance AWSPager ListAuditSuppressions where
  page rq rs
    | stop (rs ^. lasrsNextToken) = Nothing
    | stop (rs ^. lasrsSuppressions) = Nothing
    | otherwise = Just $ rq & lasNextToken .~ rs ^. lasrsNextToken

instance AWSRequest ListAuditSuppressions where
  type Rs ListAuditSuppressions = ListAuditSuppressionsResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          ListAuditSuppressionsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "suppressions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAuditSuppressions

instance NFData ListAuditSuppressions

instance ToHeaders ListAuditSuppressions where
  toHeaders = const mempty

instance ToJSON ListAuditSuppressions where
  toJSON ListAuditSuppressions' {..} =
    object
      ( catMaybes
          [ ("checkName" .=) <$> _lasCheckName,
            ("nextToken" .=) <$> _lasNextToken,
            ("ascendingOrder" .=) <$> _lasAscendingOrder,
            ("maxResults" .=) <$> _lasMaxResults,
            ("resourceIdentifier" .=) <$> _lasResourceIdentifier
          ]
      )

instance ToPath ListAuditSuppressions where
  toPath = const "/audit/suppressions/list"

instance ToQuery ListAuditSuppressions where
  toQuery = const mempty

-- | /See:/ 'listAuditSuppressionsResponse' smart constructor.
data ListAuditSuppressionsResponse = ListAuditSuppressionsResponse'
  { _lasrsNextToken ::
      !(Maybe Text),
    _lasrsSuppressions ::
      !(Maybe [AuditSuppression]),
    _lasrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditSuppressionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lasrsSuppressions' - List of audit suppressions.
--
-- * 'lasrsResponseStatus' - -- | The response status code.
listAuditSuppressionsResponse ::
  -- | 'lasrsResponseStatus'
  Int ->
  ListAuditSuppressionsResponse
listAuditSuppressionsResponse pResponseStatus_ =
  ListAuditSuppressionsResponse'
    { _lasrsNextToken = Nothing,
      _lasrsSuppressions = Nothing,
      _lasrsResponseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lasrsNextToken :: Lens' ListAuditSuppressionsResponse (Maybe Text)
lasrsNextToken = lens _lasrsNextToken (\s a -> s {_lasrsNextToken = a})

-- | List of audit suppressions.
lasrsSuppressions :: Lens' ListAuditSuppressionsResponse [AuditSuppression]
lasrsSuppressions = lens _lasrsSuppressions (\s a -> s {_lasrsSuppressions = a}) . _Default . _Coerce

-- | -- | The response status code.
lasrsResponseStatus :: Lens' ListAuditSuppressionsResponse Int
lasrsResponseStatus = lens _lasrsResponseStatus (\s a -> s {_lasrsResponseStatus = a})

instance NFData ListAuditSuppressionsResponse
