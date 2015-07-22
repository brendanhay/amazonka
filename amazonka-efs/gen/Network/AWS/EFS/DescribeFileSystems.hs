{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeFileSystems
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS file system if either
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
module Network.AWS.EFS.DescribeFileSystems
    (
    -- * Request
      DescribeFileSystems
    -- ** Request constructor
    , describeFileSystems
    -- ** Request lenses
    , dfsrqFileSystemId
    , dfsrqMaxItems
    , dfsrqCreationToken
    , dfsrqMarker

    -- * Response
    , DescribeFileSystemsResponse
    -- ** Response constructor
    , describeFileSystemsResponse
    -- ** Response lenses
    , dfsrsFileSystems
    , dfsrsMarker
    , dfsrsNextMarker
    , dfsrsStatus
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeFileSystems' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfsrqFileSystemId'
--
-- * 'dfsrqMaxItems'
--
-- * 'dfsrqCreationToken'
--
-- * 'dfsrqMarker'
data DescribeFileSystems = DescribeFileSystems'
    { _dfsrqFileSystemId  :: !(Maybe Text)
    , _dfsrqMaxItems      :: !(Maybe Nat)
    , _dfsrqCreationToken :: !(Maybe Text)
    , _dfsrqMarker        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFileSystems' smart constructor.
describeFileSystems :: DescribeFileSystems
describeFileSystems =
    DescribeFileSystems'
    { _dfsrqFileSystemId = Nothing
    , _dfsrqMaxItems = Nothing
    , _dfsrqCreationToken = Nothing
    , _dfsrqMarker = Nothing
    }

-- | Optional string. File system ID whose description you want to retrieve.
dfsrqFileSystemId :: Lens' DescribeFileSystems (Maybe Text)
dfsrqFileSystemId = lens _dfsrqFileSystemId (\ s a -> s{_dfsrqFileSystemId = a});

-- | Optional integer. Specifies the maximum number of file systems to return
-- in the response. This parameter value must be greater than 0. The number
-- of items Amazon EFS returns will be the minimum of the @MaxItems@
-- parameter specified in the request and the service\'s internal maximum
-- number of items per page.
dfsrqMaxItems :: Lens' DescribeFileSystems (Maybe Natural)
dfsrqMaxItems = lens _dfsrqMaxItems (\ s a -> s{_dfsrqMaxItems = a}) . mapping _Nat;

-- | Optional string. Restricts the list to the file system with this
-- creation token (you specify a creation token at the time of creating an
-- Amazon EFS file system).
dfsrqCreationToken :: Lens' DescribeFileSystems (Maybe Text)
dfsrqCreationToken = lens _dfsrqCreationToken (\ s a -> s{_dfsrqCreationToken = a});

-- | Optional string. Opaque pagination token returned from a previous
-- @DescribeFileSystems@ operation. If present, specifies to continue the
-- list from where the returning call had left off.
dfsrqMarker :: Lens' DescribeFileSystems (Maybe Text)
dfsrqMarker = lens _dfsrqMarker (\ s a -> s{_dfsrqMarker = a});

instance AWSRequest DescribeFileSystems where
        type Sv DescribeFileSystems = EFS
        type Rs DescribeFileSystems =
             DescribeFileSystemsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFileSystemsResponse' <$>
                   (x .?> "FileSystems" .!@ mempty) <*> (x .?> "Marker")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeFileSystems where
        toHeaders = const mempty

instance ToPath DescribeFileSystems where
        toPath = const "/2015-02-01/file-systems"

instance ToQuery DescribeFileSystems where
        toQuery DescribeFileSystems'{..}
          = mconcat
              ["FileSystemId" =: _dfsrqFileSystemId,
               "MaxItems" =: _dfsrqMaxItems,
               "CreationToken" =: _dfsrqCreationToken,
               "Marker" =: _dfsrqMarker]

-- | /See:/ 'describeFileSystemsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfsrsFileSystems'
--
-- * 'dfsrsMarker'
--
-- * 'dfsrsNextMarker'
--
-- * 'dfsrsStatus'
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'
    { _dfsrsFileSystems :: !(Maybe [FileSystemDescription])
    , _dfsrsMarker      :: !(Maybe Text)
    , _dfsrsNextMarker  :: !(Maybe Text)
    , _dfsrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFileSystemsResponse' smart constructor.
describeFileSystemsResponse :: Int -> DescribeFileSystemsResponse
describeFileSystemsResponse pStatus_ =
    DescribeFileSystemsResponse'
    { _dfsrsFileSystems = Nothing
    , _dfsrsMarker = Nothing
    , _dfsrsNextMarker = Nothing
    , _dfsrsStatus = pStatus_
    }

-- | An array of file system descriptions.
dfsrsFileSystems :: Lens' DescribeFileSystemsResponse [FileSystemDescription]
dfsrsFileSystems = lens _dfsrsFileSystems (\ s a -> s{_dfsrsFileSystems = a}) . _Default;

-- | A string, present if provided by caller in the request.
dfsrsMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrsMarker = lens _dfsrsMarker (\ s a -> s{_dfsrsMarker = a});

-- | A string, present if there are more file systems than returned in the
-- response. You can use the @NextMarker@ in the subsequent request to
-- fetch the descriptions.
dfsrsNextMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrsNextMarker = lens _dfsrsNextMarker (\ s a -> s{_dfsrsNextMarker = a});

-- | FIXME: Undocumented member.
dfsrsStatus :: Lens' DescribeFileSystemsResponse Int
dfsrsStatus = lens _dfsrsStatus (\ s a -> s{_dfsrsStatus = a});
