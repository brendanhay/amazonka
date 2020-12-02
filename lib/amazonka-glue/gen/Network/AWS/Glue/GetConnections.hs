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
-- Module      : Network.AWS.Glue.GetConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connection definitions from the Data Catalog.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetConnections
  ( -- * Creating a Request
    getConnections,
    GetConnections,

    -- * Request Lenses
    gcsCatalogId,
    gcsNextToken,
    gcsHidePassword,
    gcsFilter,
    gcsMaxResults,

    -- * Destructuring the Response
    getConnectionsResponse,
    GetConnectionsResponse,

    -- * Response Lenses
    ggrsNextToken,
    ggrsConnectionList,
    ggrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnections' smart constructor.
data GetConnections = GetConnections'
  { _gcsCatalogId ::
      !(Maybe Text),
    _gcsNextToken :: !(Maybe Text),
    _gcsHidePassword :: !(Maybe Bool),
    _gcsFilter :: !(Maybe GetConnectionsFilter),
    _gcsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsCatalogId' - The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
--
-- * 'gcsNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gcsHidePassword' - Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
--
-- * 'gcsFilter' - A filter that controls which connections are returned.
--
-- * 'gcsMaxResults' - The maximum number of connections to return in one response.
getConnections ::
  GetConnections
getConnections =
  GetConnections'
    { _gcsCatalogId = Nothing,
      _gcsNextToken = Nothing,
      _gcsHidePassword = Nothing,
      _gcsFilter = Nothing,
      _gcsMaxResults = Nothing
    }

-- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
gcsCatalogId :: Lens' GetConnections (Maybe Text)
gcsCatalogId = lens _gcsCatalogId (\s a -> s {_gcsCatalogId = a})

-- | A continuation token, if this is a continuation call.
gcsNextToken :: Lens' GetConnections (Maybe Text)
gcsNextToken = lens _gcsNextToken (\s a -> s {_gcsNextToken = a})

-- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
gcsHidePassword :: Lens' GetConnections (Maybe Bool)
gcsHidePassword = lens _gcsHidePassword (\s a -> s {_gcsHidePassword = a})

-- | A filter that controls which connections are returned.
gcsFilter :: Lens' GetConnections (Maybe GetConnectionsFilter)
gcsFilter = lens _gcsFilter (\s a -> s {_gcsFilter = a})

-- | The maximum number of connections to return in one response.
gcsMaxResults :: Lens' GetConnections (Maybe Natural)
gcsMaxResults = lens _gcsMaxResults (\s a -> s {_gcsMaxResults = a}) . mapping _Nat

instance AWSPager GetConnections where
  page rq rs
    | stop (rs ^. ggrsNextToken) = Nothing
    | stop (rs ^. ggrsConnectionList) = Nothing
    | otherwise = Just $ rq & gcsNextToken .~ rs ^. ggrsNextToken

instance AWSRequest GetConnections where
  type Rs GetConnections = GetConnectionsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ConnectionList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetConnections

instance NFData GetConnections

instance ToHeaders GetConnections where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetConnections" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConnections where
  toJSON GetConnections' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gcsCatalogId,
            ("NextToken" .=) <$> _gcsNextToken,
            ("HidePassword" .=) <$> _gcsHidePassword,
            ("Filter" .=) <$> _gcsFilter,
            ("MaxResults" .=) <$> _gcsMaxResults
          ]
      )

instance ToPath GetConnections where
  toPath = const "/"

instance ToQuery GetConnections where
  toQuery = const mempty

-- | /See:/ 'getConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { _ggrsNextToken ::
      !(Maybe Text),
    _ggrsConnectionList :: !(Maybe [Connection]),
    _ggrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsNextToken' - A continuation token, if the list of connections returned does not include the last of the filtered connections.
--
-- * 'ggrsConnectionList' - A list of requested connection definitions.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getConnectionsResponse ::
  -- | 'ggrsResponseStatus'
  Int ->
  GetConnectionsResponse
getConnectionsResponse pResponseStatus_ =
  GetConnectionsResponse'
    { _ggrsNextToken = Nothing,
      _ggrsConnectionList = Nothing,
      _ggrsResponseStatus = pResponseStatus_
    }

-- | A continuation token, if the list of connections returned does not include the last of the filtered connections.
ggrsNextToken :: Lens' GetConnectionsResponse (Maybe Text)
ggrsNextToken = lens _ggrsNextToken (\s a -> s {_ggrsNextToken = a})

-- | A list of requested connection definitions.
ggrsConnectionList :: Lens' GetConnectionsResponse [Connection]
ggrsConnectionList = lens _ggrsConnectionList (\s a -> s {_ggrsConnectionList = a}) . _Default . _Coerce

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetConnectionsResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\s a -> s {_ggrsResponseStatus = a})

instance NFData GetConnectionsResponse
