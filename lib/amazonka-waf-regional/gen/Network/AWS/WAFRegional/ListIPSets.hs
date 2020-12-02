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
-- Module      : Network.AWS.WAFRegional.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'IPSetSummary' objects in the response.
module Network.AWS.WAFRegional.ListIPSets
  ( -- * Creating a Request
    listIPSets,
    ListIPSets,

    -- * Request Lenses
    lisNextMarker,
    lisLimit,

    -- * Destructuring the Response
    listIPSetsResponse,
    ListIPSetsResponse,

    -- * Response Lenses
    lisrsNextMarker,
    lisrsIPSets,
    lisrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'listIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { _lisNextMarker :: !(Maybe Text),
    _lisLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListIPSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisNextMarker' - AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
--
-- * 'lisLimit' - Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
listIPSets ::
  ListIPSets
listIPSets =
  ListIPSets' {_lisNextMarker = Nothing, _lisLimit = Nothing}

-- | AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
lisNextMarker :: Lens' ListIPSets (Maybe Text)
lisNextMarker = lens _lisNextMarker (\s a -> s {_lisNextMarker = a})

-- | Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
lisLimit :: Lens' ListIPSets (Maybe Natural)
lisLimit = lens _lisLimit (\s a -> s {_lisLimit = a}) . mapping _Nat

instance AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request = postJSON wAFRegional
  response =
    receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            <$> (x .?> "NextMarker")
            <*> (x .?> "IPSets" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListIPSets

instance NFData ListIPSets

instance ToHeaders ListIPSets where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSWAF_Regional_20161128.ListIPSets" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListIPSets where
  toJSON ListIPSets' {..} =
    object
      ( catMaybes
          [("NextMarker" .=) <$> _lisNextMarker, ("Limit" .=) <$> _lisLimit]
      )

instance ToPath ListIPSets where
  toPath = const "/"

instance ToQuery ListIPSets where
  toQuery = const mempty

-- | /See:/ 'listIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { _lisrsNextMarker ::
      !(Maybe Text),
    _lisrsIPSets :: !(Maybe [IPSetSummary]),
    _lisrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListIPSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisrsNextMarker' - To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
--
-- * 'lisrsIPSets' - An array of 'IPSetSummary' objects.
--
-- * 'lisrsResponseStatus' - -- | The response status code.
listIPSetsResponse ::
  -- | 'lisrsResponseStatus'
  Int ->
  ListIPSetsResponse
listIPSetsResponse pResponseStatus_ =
  ListIPSetsResponse'
    { _lisrsNextMarker = Nothing,
      _lisrsIPSets = Nothing,
      _lisrsResponseStatus = pResponseStatus_
    }

-- | To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
lisrsNextMarker :: Lens' ListIPSetsResponse (Maybe Text)
lisrsNextMarker = lens _lisrsNextMarker (\s a -> s {_lisrsNextMarker = a})

-- | An array of 'IPSetSummary' objects.
lisrsIPSets :: Lens' ListIPSetsResponse [IPSetSummary]
lisrsIPSets = lens _lisrsIPSets (\s a -> s {_lisrsIPSets = a}) . _Default . _Coerce

-- | -- | The response status code.
lisrsResponseStatus :: Lens' ListIPSetsResponse Int
lisrsResponseStatus = lens _lisrsResponseStatus (\s a -> s {_lisrsResponseStatus = a})

instance NFData ListIPSetsResponse
