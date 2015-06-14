{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticFileSystem.DescribeFileSystems
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the description of a specific Amazon EFS file system if either
-- the file system @CreationToken@ or the @FileSystemId@ is provided;
-- otherwise, returns descriptions of all file systems owned by the
-- caller\'s AWS account in the AWS region of the endpoint that you\'re
-- calling.
--
-- When retrieving all file system descriptions, you can optionally specify
-- the @MaxItems@ parameter to limit the number of descriptions in a
-- response. If more file system descriptions remain, Amazon EFS returns a
-- @NextMarker@, an opaque token, in the response. In this case, you should
-- send a subsequent request with the @Marker@ request parameter set to the
-- value of @NextMarker@.
--
-- So to retrieve a list of your file system descriptions, the expected
-- usage of this API is an iterative process of first calling
-- @DescribeFileSystems@ without the @Marker@ and then continuing to call
-- it with the @Marker@ parameter set to the value of the @NextMarker@ from
-- the previous response until the response has no @NextMarker@.
--
-- Note that the implementation may return fewer than @MaxItems@ file
-- system descriptions while still including a @NextMarker@ value.
--
-- The order of file systems returned in the response of one
-- @DescribeFileSystems@ call, and the order of file systems returned
-- across the responses of a multi-call iteration, is unspecified.
--
-- This operation requires permission for the
-- @elasticfilesystem:DescribeFileSystems@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeFileSystems.html>
module Network.AWS.ElasticFileSystem.DescribeFileSystems
    (
    -- * Request
      DescribeFileSystems
    -- ** Request constructor
    , describeFileSystems
    -- ** Request lenses
    , dfsFileSystemId
    , dfsMaxItems
    , dfsCreationToken
    , dfsMarker

    -- * Response
    , DescribeFileSystemsResponse
    -- ** Response constructor
    , describeFileSystemsResponse
    -- ** Response lenses
    , dfsrFileSystems
    , dfsrMarker
    , dfsrNextMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticFileSystem.Types

-- | /See:/ 'describeFileSystems' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfsFileSystemId'
--
-- * 'dfsMaxItems'
--
-- * 'dfsCreationToken'
--
-- * 'dfsMarker'
data DescribeFileSystems = DescribeFileSystems'{_dfsFileSystemId :: Maybe Text, _dfsMaxItems :: Maybe Nat, _dfsCreationToken :: Maybe Text, _dfsMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeFileSystems' smart constructor.
describeFileSystems :: DescribeFileSystems
describeFileSystems = DescribeFileSystems'{_dfsFileSystemId = Nothing, _dfsMaxItems = Nothing, _dfsCreationToken = Nothing, _dfsMarker = Nothing};

-- | Optional string. File system ID whose description you want to retrieve.
dfsFileSystemId :: Lens' DescribeFileSystems (Maybe Text)
dfsFileSystemId = lens _dfsFileSystemId (\ s a -> s{_dfsFileSystemId = a});

-- | Optional integer. Specifies the maximum number of file systems to return
-- in the response. This parameter value must be greater than 0. The number
-- of items Amazon EFS returns will be the minimum of the @MaxItems@
-- parameter specified in the request and the service\'s internal maximum
-- number of items per page.
dfsMaxItems :: Lens' DescribeFileSystems (Maybe Natural)
dfsMaxItems = lens _dfsMaxItems (\ s a -> s{_dfsMaxItems = a}) . mapping _Nat;

-- | Optional string. Restricts the list to the file system with this
-- creation token (you specify a creation token at the time of creating an
-- Amazon EFS file system).
dfsCreationToken :: Lens' DescribeFileSystems (Maybe Text)
dfsCreationToken = lens _dfsCreationToken (\ s a -> s{_dfsCreationToken = a});

-- | Optional string. Opaque pagination token returned from a previous
-- @DescribeFileSystems@ operation. If present, specifies to continue the
-- list from where the returning call had left off.
dfsMarker :: Lens' DescribeFileSystems (Maybe Text)
dfsMarker = lens _dfsMarker (\ s a -> s{_dfsMarker = a});

instance AWSRequest DescribeFileSystems where
        type Sv DescribeFileSystems = ElasticFileSystem
        type Rs DescribeFileSystems =
             DescribeFileSystemsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFileSystemsResponse' <$>
                   x .?> "FileSystems" .!@ mempty <*> x .?> "Marker" <*>
                     x .?> "NextMarker")

instance ToHeaders DescribeFileSystems where
        toHeaders = const mempty

instance ToPath DescribeFileSystems where
        toPath = const "/2015-02-01/file-systems"

instance ToQuery DescribeFileSystems where
        toQuery DescribeFileSystems'{..}
          = mconcat
              ["FileSystemId" =: _dfsFileSystemId,
               "MaxItems" =: _dfsMaxItems,
               "CreationToken" =: _dfsCreationToken,
               "Marker" =: _dfsMarker]

-- | /See:/ 'describeFileSystemsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfsrFileSystems'
--
-- * 'dfsrMarker'
--
-- * 'dfsrNextMarker'
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'{_dfsrFileSystems :: Maybe [FileSystemDescription], _dfsrMarker :: Maybe Text, _dfsrNextMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeFileSystemsResponse' smart constructor.
describeFileSystemsResponse :: DescribeFileSystemsResponse
describeFileSystemsResponse = DescribeFileSystemsResponse'{_dfsrFileSystems = Nothing, _dfsrMarker = Nothing, _dfsrNextMarker = Nothing};

-- | An array of file system descriptions.
dfsrFileSystems :: Lens' DescribeFileSystemsResponse (Maybe [FileSystemDescription])
dfsrFileSystems = lens _dfsrFileSystems (\ s a -> s{_dfsrFileSystems = a});

-- | A string, present if provided by caller in the request.
dfsrMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrMarker = lens _dfsrMarker (\ s a -> s{_dfsrMarker = a});

-- | A string, present if there are more file systems than returned in the
-- response. You can use the @NextMarker@ in the subsequent request to
-- fetch the descriptions.
dfsrNextMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrNextMarker = lens _dfsrNextMarker (\ s a -> s{_dfsrNextMarker = a});
