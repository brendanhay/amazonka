{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeFileSystems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS file system if either the file system @CreationToken@ or the @FileSystemId@ is provided. Otherwise, it returns descriptions of all file systems owned by the caller's AWS account in the AWS Region of the endpoint that you're calling.
--
--
-- When retrieving all file system descriptions, you can optionally specify the @MaxItems@ parameter to limit the number of descriptions in a response. If more file system descriptions remain, Amazon EFS returns a @NextMarker@ , an opaque token, in the response. In this case, you should send a subsequent request with the @Marker@ request parameter set to the value of @NextMarker@ .
--
-- To retrieve a list of your file system descriptions, this operation is used in an iterative process, where @DescribeFileSystems@ is called first without the @Marker@ and then the operation continues to call it with the @Marker@ parameter set to the value of the @NextMarker@ from the previous response until the response has no @NextMarker@ .
--
-- The implementation may return fewer than @MaxItems@ file system descriptions while still including a @NextMarker@ value.
--
-- The order of file systems returned in the response of one @DescribeFileSystems@ call and the order of file systems returned across the responses of a multi-call iteration is unspecified.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeFileSystems@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeFileSystems
    (
    -- * Creating a Request
      describeFileSystems
    , DescribeFileSystems
    -- * Request Lenses
    , dfsFileSystemId
    , dfsCreationToken
    , dfsMarker
    , dfsMaxItems

    -- * Destructuring the Response
    , describeFileSystemsResponse
    , DescribeFileSystemsResponse
    -- * Response Lenses
    , dfsrsFileSystems
    , dfsrsMarker
    , dfsrsNextMarker
    , dfsrsResponseStatus
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeFileSystems' smart constructor.
data DescribeFileSystems = DescribeFileSystems'
  { _dfsFileSystemId  :: !(Maybe Text)
  , _dfsCreationToken :: !(Maybe Text)
  , _dfsMarker        :: !(Maybe Text)
  , _dfsMaxItems      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFileSystems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsFileSystemId' - (Optional) ID of the file system whose description you want to retrieve (String).
--
-- * 'dfsCreationToken' - (Optional) Restricts the list to the file system with this creation token (String). You specify a creation token when you create an Amazon EFS file system.
--
-- * 'dfsMarker' - (Optional) Opaque pagination token returned from a previous @DescribeFileSystems@ operation (String). If present, specifies to continue the list from where the returning call had left off.
--
-- * 'dfsMaxItems' - (Optional) Specifies the maximum number of file systems to return in the response (integer). This parameter value must be greater than 0. The number of items that Amazon EFS returns is the minimum of the @MaxItems@ parameter specified in the request and the service's internal maximum number of items per page.
describeFileSystems
    :: DescribeFileSystems
describeFileSystems =
  DescribeFileSystems'
    { _dfsFileSystemId = Nothing
    , _dfsCreationToken = Nothing
    , _dfsMarker = Nothing
    , _dfsMaxItems = Nothing
    }


-- | (Optional) ID of the file system whose description you want to retrieve (String).
dfsFileSystemId :: Lens' DescribeFileSystems (Maybe Text)
dfsFileSystemId = lens _dfsFileSystemId (\ s a -> s{_dfsFileSystemId = a})

-- | (Optional) Restricts the list to the file system with this creation token (String). You specify a creation token when you create an Amazon EFS file system.
dfsCreationToken :: Lens' DescribeFileSystems (Maybe Text)
dfsCreationToken = lens _dfsCreationToken (\ s a -> s{_dfsCreationToken = a})

-- | (Optional) Opaque pagination token returned from a previous @DescribeFileSystems@ operation (String). If present, specifies to continue the list from where the returning call had left off.
dfsMarker :: Lens' DescribeFileSystems (Maybe Text)
dfsMarker = lens _dfsMarker (\ s a -> s{_dfsMarker = a})

-- | (Optional) Specifies the maximum number of file systems to return in the response (integer). This parameter value must be greater than 0. The number of items that Amazon EFS returns is the minimum of the @MaxItems@ parameter specified in the request and the service's internal maximum number of items per page.
dfsMaxItems :: Lens' DescribeFileSystems (Maybe Natural)
dfsMaxItems = lens _dfsMaxItems (\ s a -> s{_dfsMaxItems = a}) . mapping _Nat

instance AWSPager DescribeFileSystems where
        page rq rs
          | stop (rs ^. dfsrsNextMarker) = Nothing
          | stop (rs ^. dfsrsFileSystems) = Nothing
          | otherwise =
            Just $ rq & dfsMarker .~ rs ^. dfsrsNextMarker

instance AWSRequest DescribeFileSystems where
        type Rs DescribeFileSystems =
             DescribeFileSystemsResponse
        request = get efs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFileSystemsResponse' <$>
                   (x .?> "FileSystems" .!@ mempty) <*> (x .?> "Marker")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFileSystems where

instance NFData DescribeFileSystems where

instance ToHeaders DescribeFileSystems where
        toHeaders = const mempty

instance ToPath DescribeFileSystems where
        toPath = const "/2015-02-01/file-systems"

instance ToQuery DescribeFileSystems where
        toQuery DescribeFileSystems'{..}
          = mconcat
              ["FileSystemId" =: _dfsFileSystemId,
               "CreationToken" =: _dfsCreationToken,
               "Marker" =: _dfsMarker, "MaxItems" =: _dfsMaxItems]

-- | /See:/ 'describeFileSystemsResponse' smart constructor.
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'
  { _dfsrsFileSystems    :: !(Maybe [FileSystemDescription])
  , _dfsrsMarker         :: !(Maybe Text)
  , _dfsrsNextMarker     :: !(Maybe Text)
  , _dfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFileSystemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsFileSystems' - Array of file system descriptions.
--
-- * 'dfsrsMarker' - Present if provided by caller in the request (String).
--
-- * 'dfsrsNextMarker' - Present if there are more file systems than returned in the response (String). You can use the @NextMarker@ in the subsequent request to fetch the descriptions.
--
-- * 'dfsrsResponseStatus' - -- | The response status code.
describeFileSystemsResponse
    :: Int -- ^ 'dfsrsResponseStatus'
    -> DescribeFileSystemsResponse
describeFileSystemsResponse pResponseStatus_ =
  DescribeFileSystemsResponse'
    { _dfsrsFileSystems = Nothing
    , _dfsrsMarker = Nothing
    , _dfsrsNextMarker = Nothing
    , _dfsrsResponseStatus = pResponseStatus_
    }


-- | Array of file system descriptions.
dfsrsFileSystems :: Lens' DescribeFileSystemsResponse [FileSystemDescription]
dfsrsFileSystems = lens _dfsrsFileSystems (\ s a -> s{_dfsrsFileSystems = a}) . _Default . _Coerce

-- | Present if provided by caller in the request (String).
dfsrsMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrsMarker = lens _dfsrsMarker (\ s a -> s{_dfsrsMarker = a})

-- | Present if there are more file systems than returned in the response (String). You can use the @NextMarker@ in the subsequent request to fetch the descriptions.
dfsrsNextMarker :: Lens' DescribeFileSystemsResponse (Maybe Text)
dfsrsNextMarker = lens _dfsrsNextMarker (\ s a -> s{_dfsrsNextMarker = a})

-- | -- | The response status code.
dfsrsResponseStatus :: Lens' DescribeFileSystemsResponse Int
dfsrsResponseStatus = lens _dfsrsResponseStatus (\ s a -> s{_dfsrsResponseStatus = a})

instance NFData DescribeFileSystemsResponse where
