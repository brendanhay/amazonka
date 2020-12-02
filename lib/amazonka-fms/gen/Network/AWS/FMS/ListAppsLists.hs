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
-- Module      : Network.AWS.FMS.ListAppsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
module Network.AWS.FMS.ListAppsLists
  ( -- * Creating a Request
    listAppsLists,
    ListAppsLists,

    -- * Request Lenses
    lalDefaultLists,
    lalNextToken,
    lalMaxResults,

    -- * Destructuring the Response
    listAppsListsResponse,
    ListAppsListsResponse,

    -- * Response Lenses
    lalrsNextToken,
    lalrsAppsLists,
    lalrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { _lalDefaultLists ::
      !(Maybe Bool),
    _lalNextToken :: !(Maybe Text),
    _lalMaxResults :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAppsLists' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lalDefaultLists' - Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- * 'lalNextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- * 'lalMaxResults' - The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects. If you don't specify this, AWS Firewall Manager returns all available objects.
listAppsLists ::
  -- | 'lalMaxResults'
  Natural ->
  ListAppsLists
listAppsLists pMaxResults_ =
  ListAppsLists'
    { _lalDefaultLists = Nothing,
      _lalNextToken = Nothing,
      _lalMaxResults = _Nat # pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
lalDefaultLists :: Lens' ListAppsLists (Maybe Bool)
lalDefaultLists = lens _lalDefaultLists (\s a -> s {_lalDefaultLists = a})

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
lalNextToken :: Lens' ListAppsLists (Maybe Text)
lalNextToken = lens _lalNextToken (\s a -> s {_lalNextToken = a})

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects. If you don't specify this, AWS Firewall Manager returns all available objects.
lalMaxResults :: Lens' ListAppsLists Natural
lalMaxResults = lens _lalMaxResults (\s a -> s {_lalMaxResults = a}) . _Nat

instance AWSRequest ListAppsLists where
  type Rs ListAppsLists = ListAppsListsResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "AppsLists" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAppsLists

instance NFData ListAppsLists

instance ToHeaders ListAppsLists where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSFMS_20180101.ListAppsLists" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAppsLists where
  toJSON ListAppsLists' {..} =
    object
      ( catMaybes
          [ ("DefaultLists" .=) <$> _lalDefaultLists,
            ("NextToken" .=) <$> _lalNextToken,
            Just ("MaxResults" .= _lalMaxResults)
          ]
      )

instance ToPath ListAppsLists where
  toPath = const "/"

instance ToQuery ListAppsLists where
  toQuery = const mempty

-- | /See:/ 'listAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { _lalrsNextToken ::
      !(Maybe Text),
    _lalrsAppsLists ::
      !(Maybe [AppsListDataSummary]),
    _lalrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAppsListsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lalrsNextToken' - If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- * 'lalrsAppsLists' - An array of @AppsListDataSummary@ objects.
--
-- * 'lalrsResponseStatus' - -- | The response status code.
listAppsListsResponse ::
  -- | 'lalrsResponseStatus'
  Int ->
  ListAppsListsResponse
listAppsListsResponse pResponseStatus_ =
  ListAppsListsResponse'
    { _lalrsNextToken = Nothing,
      _lalrsAppsLists = Nothing,
      _lalrsResponseStatus = pResponseStatus_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
lalrsNextToken :: Lens' ListAppsListsResponse (Maybe Text)
lalrsNextToken = lens _lalrsNextToken (\s a -> s {_lalrsNextToken = a})

-- | An array of @AppsListDataSummary@ objects.
lalrsAppsLists :: Lens' ListAppsListsResponse [AppsListDataSummary]
lalrsAppsLists = lens _lalrsAppsLists (\s a -> s {_lalrsAppsLists = a}) . _Default . _Coerce

-- | -- | The response status code.
lalrsResponseStatus :: Lens' ListAppsListsResponse Int
lalrsResponseStatus = lens _lalrsResponseStatus (\s a -> s {_lalrsResponseStatus = a})

instance NFData ListAppsListsResponse
