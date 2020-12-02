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
-- Module      : Network.AWS.MediaStoreData.DescribeObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the headers for an object at the specified path.
--
--
module Network.AWS.MediaStoreData.DescribeObject
    (
    -- * Creating a Request
      describeObject
    , DescribeObject
    -- * Request Lenses
    , dPath

    -- * Destructuring the Response
    , describeObjectResponse
    , DescribeObjectResponse
    -- * Response Lenses
    , drsETag
    , drsContentLength
    , drsCacheControl
    , drsLastModified
    , drsContentType
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeObject' smart constructor.
newtype DescribeObject = DescribeObject'
  { _dPath :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPath' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
describeObject
    :: Text -- ^ 'dPath'
    -> DescribeObject
describeObject pPath_ = DescribeObject' {_dPath = pPath_}


-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
dPath :: Lens' DescribeObject Text
dPath = lens _dPath (\ s a -> s{_dPath = a})

instance AWSRequest DescribeObject where
        type Rs DescribeObject = DescribeObjectResponse
        request = head' mediaStoreData
        response
          = receiveEmpty
              (\ s h x ->
                 DescribeObjectResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Content-Length") <*>
                     (h .#? "Cache-Control")
                     <*> (h .#? "Last-Modified")
                     <*> (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeObject where

instance NFData DescribeObject where

instance ToHeaders DescribeObject where
        toHeaders = const mempty

instance ToPath DescribeObject where
        toPath DescribeObject'{..}
          = mconcat ["/", toBS _dPath]

instance ToQuery DescribeObject where
        toQuery = const mempty

-- | /See:/ 'describeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { _drsETag           :: !(Maybe Text)
  , _drsContentLength  :: !(Maybe Nat)
  , _drsCacheControl   :: !(Maybe Text)
  , _drsLastModified   :: !(Maybe POSIX)
  , _drsContentType    :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsETag' - The ETag that represents a unique instance of the object.
--
-- * 'drsContentLength' - The length of the object in bytes.
--
-- * 'drsCacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
--
-- * 'drsLastModified' - The date and time that the object was last modified.
--
-- * 'drsContentType' - The content type of the object.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeObjectResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeObjectResponse
describeObjectResponse pResponseStatus_ =
  DescribeObjectResponse'
    { _drsETag = Nothing
    , _drsContentLength = Nothing
    , _drsCacheControl = Nothing
    , _drsLastModified = Nothing
    , _drsContentType = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The ETag that represents a unique instance of the object.
drsETag :: Lens' DescribeObjectResponse (Maybe Text)
drsETag = lens _drsETag (\ s a -> s{_drsETag = a})

-- | The length of the object in bytes.
drsContentLength :: Lens' DescribeObjectResponse (Maybe Natural)
drsContentLength = lens _drsContentLength (\ s a -> s{_drsContentLength = a}) . mapping _Nat

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
drsCacheControl :: Lens' DescribeObjectResponse (Maybe Text)
drsCacheControl = lens _drsCacheControl (\ s a -> s{_drsCacheControl = a})

-- | The date and time that the object was last modified.
drsLastModified :: Lens' DescribeObjectResponse (Maybe UTCTime)
drsLastModified = lens _drsLastModified (\ s a -> s{_drsLastModified = a}) . mapping _Time

-- | The content type of the object.
drsContentType :: Lens' DescribeObjectResponse (Maybe Text)
drsContentType = lens _drsContentType (\ s a -> s{_drsContentType = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeObjectResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeObjectResponse where
