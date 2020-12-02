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
-- Module      : Network.AWS.ElasticSearch.GetPackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of versions of the package, along with their creation time and commit message.
module Network.AWS.ElasticSearch.GetPackageVersionHistory
  ( -- * Creating a Request
    getPackageVersionHistory,
    GetPackageVersionHistory,

    -- * Request Lenses
    gpvhNextToken,
    gpvhMaxResults,
    gpvhPackageId,

    -- * Destructuring the Response
    getPackageVersionHistoryResponse,
    GetPackageVersionHistoryResponse,

    -- * Response Lenses
    gpvhrsPackageId,
    gpvhrsPackageVersionHistoryList,
    gpvhrsNextToken,
    gpvhrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'GetPackageVersionHistory' @ operation.
--
--
--
-- /See:/ 'getPackageVersionHistory' smart constructor.
data GetPackageVersionHistory = GetPackageVersionHistory'
  { _gpvhNextToken ::
      !(Maybe Text),
    _gpvhMaxResults :: !(Maybe Int),
    _gpvhPackageId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPackageVersionHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpvhNextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- * 'gpvhMaxResults' - Limits results to a maximum number of versions.
--
-- * 'gpvhPackageId' - Returns an audit history of versions of the package.
getPackageVersionHistory ::
  -- | 'gpvhPackageId'
  Text ->
  GetPackageVersionHistory
getPackageVersionHistory pPackageId_ =
  GetPackageVersionHistory'
    { _gpvhNextToken = Nothing,
      _gpvhMaxResults = Nothing,
      _gpvhPackageId = pPackageId_
    }

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
gpvhNextToken :: Lens' GetPackageVersionHistory (Maybe Text)
gpvhNextToken = lens _gpvhNextToken (\s a -> s {_gpvhNextToken = a})

-- | Limits results to a maximum number of versions.
gpvhMaxResults :: Lens' GetPackageVersionHistory (Maybe Int)
gpvhMaxResults = lens _gpvhMaxResults (\s a -> s {_gpvhMaxResults = a})

-- | Returns an audit history of versions of the package.
gpvhPackageId :: Lens' GetPackageVersionHistory Text
gpvhPackageId = lens _gpvhPackageId (\s a -> s {_gpvhPackageId = a})

instance AWSRequest GetPackageVersionHistory where
  type Rs GetPackageVersionHistory = GetPackageVersionHistoryResponse
  request = get elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          GetPackageVersionHistoryResponse'
            <$> (x .?> "PackageID")
            <*> (x .?> "PackageVersionHistoryList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetPackageVersionHistory

instance NFData GetPackageVersionHistory

instance ToHeaders GetPackageVersionHistory where
  toHeaders = const mempty

instance ToPath GetPackageVersionHistory where
  toPath GetPackageVersionHistory' {..} =
    mconcat
      ["/2015-01-01/packages/", toBS _gpvhPackageId, "/history"]

instance ToQuery GetPackageVersionHistory where
  toQuery GetPackageVersionHistory' {..} =
    mconcat
      ["nextToken" =: _gpvhNextToken, "maxResults" =: _gpvhMaxResults]

-- | Container for response returned by @'GetPackageVersionHistory' @ operation.
--
--
--
-- /See:/ 'getPackageVersionHistoryResponse' smart constructor.
data GetPackageVersionHistoryResponse = GetPackageVersionHistoryResponse'
  { _gpvhrsPackageId ::
      !(Maybe Text),
    _gpvhrsPackageVersionHistoryList ::
      !( Maybe
           [PackageVersionHistory]
       ),
    _gpvhrsNextToken ::
      !(Maybe Text),
    _gpvhrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPackageVersionHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpvhrsPackageId' - Undocumented member.
--
-- * 'gpvhrsPackageVersionHistoryList' - List of @PackageVersionHistory@ objects.
--
-- * 'gpvhrsNextToken' - Undocumented member.
--
-- * 'gpvhrsResponseStatus' - -- | The response status code.
getPackageVersionHistoryResponse ::
  -- | 'gpvhrsResponseStatus'
  Int ->
  GetPackageVersionHistoryResponse
getPackageVersionHistoryResponse pResponseStatus_ =
  GetPackageVersionHistoryResponse'
    { _gpvhrsPackageId = Nothing,
      _gpvhrsPackageVersionHistoryList = Nothing,
      _gpvhrsNextToken = Nothing,
      _gpvhrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gpvhrsPackageId :: Lens' GetPackageVersionHistoryResponse (Maybe Text)
gpvhrsPackageId = lens _gpvhrsPackageId (\s a -> s {_gpvhrsPackageId = a})

-- | List of @PackageVersionHistory@ objects.
gpvhrsPackageVersionHistoryList :: Lens' GetPackageVersionHistoryResponse [PackageVersionHistory]
gpvhrsPackageVersionHistoryList = lens _gpvhrsPackageVersionHistoryList (\s a -> s {_gpvhrsPackageVersionHistoryList = a}) . _Default . _Coerce

-- | Undocumented member.
gpvhrsNextToken :: Lens' GetPackageVersionHistoryResponse (Maybe Text)
gpvhrsNextToken = lens _gpvhrsNextToken (\s a -> s {_gpvhrsNextToken = a})

-- | -- | The response status code.
gpvhrsResponseStatus :: Lens' GetPackageVersionHistoryResponse Int
gpvhrsResponseStatus = lens _gpvhrsResponseStatus (\s a -> s {_gpvhrsResponseStatus = a})

instance NFData GetPackageVersionHistoryResponse
