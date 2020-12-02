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
-- Module      : Network.AWS.FMS.ListProtocolsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ProtocolsListDataSummary@ objects.
module Network.AWS.FMS.ListProtocolsLists
  ( -- * Creating a Request
    listProtocolsLists,
    ListProtocolsLists,

    -- * Request Lenses
    lplDefaultLists,
    lplNextToken,
    lplMaxResults,

    -- * Destructuring the Response
    listProtocolsListsResponse,
    ListProtocolsListsResponse,

    -- * Response Lenses
    lplrsProtocolsLists,
    lplrsNextToken,
    lplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProtocolsLists' smart constructor.
data ListProtocolsLists = ListProtocolsLists'
  { _lplDefaultLists ::
      !(Maybe Bool),
    _lplNextToken :: !(Maybe Text),
    _lplMaxResults :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProtocolsLists' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lplDefaultLists' - Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- * 'lplNextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- * 'lplMaxResults' - The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects. If you don't specify this, AWS Firewall Manager returns all available objects.
listProtocolsLists ::
  -- | 'lplMaxResults'
  Natural ->
  ListProtocolsLists
listProtocolsLists pMaxResults_ =
  ListProtocolsLists'
    { _lplDefaultLists = Nothing,
      _lplNextToken = Nothing,
      _lplMaxResults = _Nat # pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
lplDefaultLists :: Lens' ListProtocolsLists (Maybe Bool)
lplDefaultLists = lens _lplDefaultLists (\s a -> s {_lplDefaultLists = a})

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
lplNextToken :: Lens' ListProtocolsLists (Maybe Text)
lplNextToken = lens _lplNextToken (\s a -> s {_lplNextToken = a})

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects. If you don't specify this, AWS Firewall Manager returns all available objects.
lplMaxResults :: Lens' ListProtocolsLists Natural
lplMaxResults = lens _lplMaxResults (\s a -> s {_lplMaxResults = a}) . _Nat

instance AWSRequest ListProtocolsLists where
  type Rs ListProtocolsLists = ListProtocolsListsResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          ListProtocolsListsResponse'
            <$> (x .?> "ProtocolsLists" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListProtocolsLists

instance NFData ListProtocolsLists

instance ToHeaders ListProtocolsLists where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.ListProtocolsLists" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListProtocolsLists where
  toJSON ListProtocolsLists' {..} =
    object
      ( catMaybes
          [ ("DefaultLists" .=) <$> _lplDefaultLists,
            ("NextToken" .=) <$> _lplNextToken,
            Just ("MaxResults" .= _lplMaxResults)
          ]
      )

instance ToPath ListProtocolsLists where
  toPath = const "/"

instance ToQuery ListProtocolsLists where
  toQuery = const mempty

-- | /See:/ 'listProtocolsListsResponse' smart constructor.
data ListProtocolsListsResponse = ListProtocolsListsResponse'
  { _lplrsProtocolsLists ::
      !(Maybe [ProtocolsListDataSummary]),
    _lplrsNextToken :: !(Maybe Text),
    _lplrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProtocolsListsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lplrsProtocolsLists' - An array of @ProtocolsListDataSummary@ objects.
--
-- * 'lplrsNextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- * 'lplrsResponseStatus' - -- | The response status code.
listProtocolsListsResponse ::
  -- | 'lplrsResponseStatus'
  Int ->
  ListProtocolsListsResponse
listProtocolsListsResponse pResponseStatus_ =
  ListProtocolsListsResponse'
    { _lplrsProtocolsLists = Nothing,
      _lplrsNextToken = Nothing,
      _lplrsResponseStatus = pResponseStatus_
    }

-- | An array of @ProtocolsListDataSummary@ objects.
lplrsProtocolsLists :: Lens' ListProtocolsListsResponse [ProtocolsListDataSummary]
lplrsProtocolsLists = lens _lplrsProtocolsLists (\s a -> s {_lplrsProtocolsLists = a}) . _Default . _Coerce

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
lplrsNextToken :: Lens' ListProtocolsListsResponse (Maybe Text)
lplrsNextToken = lens _lplrsNextToken (\s a -> s {_lplrsNextToken = a})

-- | -- | The response status code.
lplrsResponseStatus :: Lens' ListProtocolsListsResponse Int
lplrsResponseStatus = lens _lplrsResponseStatus (\s a -> s {_lplrsResponseStatus = a})

instance NFData ListProtocolsListsResponse
