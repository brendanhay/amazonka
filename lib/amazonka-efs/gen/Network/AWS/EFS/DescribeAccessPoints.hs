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
-- Module      : Network.AWS.EFS.DescribeAccessPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS access point if the @AccessPointId@ is provided. If you provide an EFS @FileSystemId@ , it returns descriptions of all access points for that file system. You can provide either an @AccessPointId@ or a @FileSystemId@ in the request, but not both.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.DescribeAccessPoints
  ( -- * Creating a Request
    describeAccessPoints,
    DescribeAccessPoints,

    -- * Request Lenses
    dapAccessPointId,
    dapFileSystemId,
    dapNextToken,
    dapMaxResults,

    -- * Destructuring the Response
    describeAccessPointsResponse,
    DescribeAccessPointsResponse,

    -- * Response Lenses
    daprsAccessPoints,
    daprsNextToken,
    daprsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { _dapAccessPointId ::
      !(Maybe Text),
    _dapFileSystemId :: !(Maybe Text),
    _dapNextToken :: !(Maybe Text),
    _dapMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccessPoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dapAccessPointId' - (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
--
-- * 'dapFileSystemId' - (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
--
-- * 'dapNextToken' - @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
--
-- * 'dapMaxResults' - (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
describeAccessPoints ::
  DescribeAccessPoints
describeAccessPoints =
  DescribeAccessPoints'
    { _dapAccessPointId = Nothing,
      _dapFileSystemId = Nothing,
      _dapNextToken = Nothing,
      _dapMaxResults = Nothing
    }

-- | (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
dapAccessPointId :: Lens' DescribeAccessPoints (Maybe Text)
dapAccessPointId = lens _dapAccessPointId (\s a -> s {_dapAccessPointId = a})

-- | (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
dapFileSystemId :: Lens' DescribeAccessPoints (Maybe Text)
dapFileSystemId = lens _dapFileSystemId (\s a -> s {_dapFileSystemId = a})

-- | @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
dapNextToken :: Lens' DescribeAccessPoints (Maybe Text)
dapNextToken = lens _dapNextToken (\s a -> s {_dapNextToken = a})

-- | (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
dapMaxResults :: Lens' DescribeAccessPoints (Maybe Natural)
dapMaxResults = lens _dapMaxResults (\s a -> s {_dapMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeAccessPoints where
  type Rs DescribeAccessPoints = DescribeAccessPointsResponse
  request = get efs
  response =
    receiveJSON
      ( \s h x ->
          DescribeAccessPointsResponse'
            <$> (x .?> "AccessPoints" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAccessPoints

instance NFData DescribeAccessPoints

instance ToHeaders DescribeAccessPoints where
  toHeaders = const mempty

instance ToPath DescribeAccessPoints where
  toPath = const "/2015-02-01/access-points"

instance ToQuery DescribeAccessPoints where
  toQuery DescribeAccessPoints' {..} =
    mconcat
      [ "AccessPointId" =: _dapAccessPointId,
        "FileSystemId" =: _dapFileSystemId,
        "NextToken" =: _dapNextToken,
        "MaxResults" =: _dapMaxResults
      ]

-- | /See:/ 'describeAccessPointsResponse' smart constructor.
data DescribeAccessPointsResponse = DescribeAccessPointsResponse'
  { _daprsAccessPoints ::
      !(Maybe [AccessPointDescription]),
    _daprsNextToken :: !(Maybe Text),
    _daprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccessPointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daprsAccessPoints' - An array of access point descriptions.
--
-- * 'daprsNextToken' - Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
--
-- * 'daprsResponseStatus' - -- | The response status code.
describeAccessPointsResponse ::
  -- | 'daprsResponseStatus'
  Int ->
  DescribeAccessPointsResponse
describeAccessPointsResponse pResponseStatus_ =
  DescribeAccessPointsResponse'
    { _daprsAccessPoints = Nothing,
      _daprsNextToken = Nothing,
      _daprsResponseStatus = pResponseStatus_
    }

-- | An array of access point descriptions.
daprsAccessPoints :: Lens' DescribeAccessPointsResponse [AccessPointDescription]
daprsAccessPoints = lens _daprsAccessPoints (\s a -> s {_daprsAccessPoints = a}) . _Default . _Coerce

-- | Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
daprsNextToken :: Lens' DescribeAccessPointsResponse (Maybe Text)
daprsNextToken = lens _daprsNextToken (\s a -> s {_daprsNextToken = a})

-- | -- | The response status code.
daprsResponseStatus :: Lens' DescribeAccessPointsResponse Int
daprsResponseStatus = lens _daprsResponseStatus (\s a -> s {_daprsResponseStatus = a})

instance NFData DescribeAccessPointsResponse
