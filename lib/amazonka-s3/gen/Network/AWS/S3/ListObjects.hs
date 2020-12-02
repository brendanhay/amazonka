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
-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjects
    (
    -- * Creating a Request
      listObjects
    , ListObjects
    -- * Request Lenses
    , loPrefix
    , loEncodingType
    , loRequestPayer
    , loMarker
    , loMaxKeys
    , loDelimiter
    , loBucket

    -- * Destructuring the Response
    , listObjectsResponse
    , ListObjectsResponse
    -- * Response Lenses
    , lorsContents
    , lorsPrefix
    , lorsCommonPrefixes
    , lorsEncodingType
    , lorsName
    , lorsMarker
    , lorsNextMarker
    , lorsMaxKeys
    , lorsIsTruncated
    , lorsDelimiter
    , lorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listObjects' smart constructor.
data ListObjects = ListObjects'
  { _loPrefix       :: !(Maybe Text)
  , _loEncodingType :: !(Maybe EncodingType)
  , _loRequestPayer :: !(Maybe RequestPayer)
  , _loMarker       :: !(Maybe Text)
  , _loMaxKeys      :: !(Maybe Int)
  , _loDelimiter    :: !(Maybe Delimiter)
  , _loBucket       :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'loEncodingType' - Undocumented member.
--
-- * 'loRequestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
--
-- * 'loMarker' - Specifies the key to start with when listing objects in a bucket.
--
-- * 'loMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'loDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'loBucket' - Undocumented member.
listObjects
    :: BucketName -- ^ 'loBucket'
    -> ListObjects
listObjects pBucket_ =
  ListObjects'
    { _loPrefix = Nothing
    , _loEncodingType = Nothing
    , _loRequestPayer = Nothing
    , _loMarker = Nothing
    , _loMaxKeys = Nothing
    , _loDelimiter = Nothing
    , _loBucket = pBucket_
    }


-- | Limits the response to keys that begin with the specified prefix.
loPrefix :: Lens' ListObjects (Maybe Text)
loPrefix = lens _loPrefix (\ s a -> s{_loPrefix = a})

-- | Undocumented member.
loEncodingType :: Lens' ListObjects (Maybe EncodingType)
loEncodingType = lens _loEncodingType (\ s a -> s{_loEncodingType = a})

-- | Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
loRequestPayer :: Lens' ListObjects (Maybe RequestPayer)
loRequestPayer = lens _loRequestPayer (\ s a -> s{_loRequestPayer = a})

-- | Specifies the key to start with when listing objects in a bucket.
loMarker :: Lens' ListObjects (Maybe Text)
loMarker = lens _loMarker (\ s a -> s{_loMarker = a})

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
loMaxKeys :: Lens' ListObjects (Maybe Int)
loMaxKeys = lens _loMaxKeys (\ s a -> s{_loMaxKeys = a})

-- | A delimiter is a character you use to group keys.
loDelimiter :: Lens' ListObjects (Maybe Delimiter)
loDelimiter = lens _loDelimiter (\ s a -> s{_loDelimiter = a})

-- | Undocumented member.
loBucket :: Lens' ListObjects BucketName
loBucket = lens _loBucket (\ s a -> s{_loBucket = a})

instance AWSPager ListObjects where
        page rq rs
          | stop (rs ^. lorsIsTruncated) = Nothing
          | isNothing
              (rs ^.
                 choice (^. lorsNextMarker)
                   (^? (lorsContents . _last . oKey)))
            = Nothing
          | otherwise =
            Just $ rq &
              loMarker .~
                rs ^.
                  choice (^. lorsNextMarker)
                    (^? (lorsContents . _last . oKey))

instance AWSRequest ListObjects where
        type Rs ListObjects = ListObjectsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListObjectsResponse' <$>
                   (may (parseXMLList "Contents") x) <*>
                     (x .@? "Prefix")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "EncodingType")
                     <*> (x .@? "Name")
                     <*> (x .@? "Marker")
                     <*> (x .@? "NextMarker")
                     <*> (x .@? "MaxKeys")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance Hashable ListObjects where

instance NFData ListObjects where

instance ToHeaders ListObjects where
        toHeaders ListObjects'{..}
          = mconcat ["x-amz-request-payer" =# _loRequestPayer]

instance ToPath ListObjects where
        toPath ListObjects'{..}
          = mconcat ["/", toBS _loBucket]

instance ToQuery ListObjects where
        toQuery ListObjects'{..}
          = mconcat
              ["prefix" =: _loPrefix,
               "encoding-type" =: _loEncodingType,
               "marker" =: _loMarker, "max-keys" =: _loMaxKeys,
               "delimiter" =: _loDelimiter]

-- | /See:/ 'listObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { _lorsContents       :: !(Maybe [Object])
  , _lorsPrefix         :: !(Maybe Text)
  , _lorsCommonPrefixes :: !(Maybe [CommonPrefix])
  , _lorsEncodingType   :: !(Maybe EncodingType)
  , _lorsName           :: !(Maybe BucketName)
  , _lorsMarker         :: !(Maybe Text)
  , _lorsNextMarker     :: !(Maybe Text)
  , _lorsMaxKeys        :: !(Maybe Int)
  , _lorsIsTruncated    :: !(Maybe Bool)
  , _lorsDelimiter      :: !(Maybe Delimiter)
  , _lorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsContents' - Undocumented member.
--
-- * 'lorsPrefix' - Undocumented member.
--
-- * 'lorsCommonPrefixes' - Undocumented member.
--
-- * 'lorsEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lorsName' - Undocumented member.
--
-- * 'lorsMarker' - Undocumented member.
--
-- * 'lorsNextMarker' - When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMaker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
--
-- * 'lorsMaxKeys' - Undocumented member.
--
-- * 'lorsIsTruncated' - A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
--
-- * 'lorsDelimiter' - Undocumented member.
--
-- * 'lorsResponseStatus' - -- | The response status code.
listObjectsResponse
    :: Int -- ^ 'lorsResponseStatus'
    -> ListObjectsResponse
listObjectsResponse pResponseStatus_ =
  ListObjectsResponse'
    { _lorsContents = Nothing
    , _lorsPrefix = Nothing
    , _lorsCommonPrefixes = Nothing
    , _lorsEncodingType = Nothing
    , _lorsName = Nothing
    , _lorsMarker = Nothing
    , _lorsNextMarker = Nothing
    , _lorsMaxKeys = Nothing
    , _lorsIsTruncated = Nothing
    , _lorsDelimiter = Nothing
    , _lorsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lorsContents :: Lens' ListObjectsResponse [Object]
lorsContents = lens _lorsContents (\ s a -> s{_lorsContents = a}) . _Default . _Coerce

-- | Undocumented member.
lorsPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorsPrefix = lens _lorsPrefix (\ s a -> s{_lorsPrefix = a})

-- | Undocumented member.
lorsCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorsCommonPrefixes = lens _lorsCommonPrefixes (\ s a -> s{_lorsCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorsEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
lorsEncodingType = lens _lorsEncodingType (\ s a -> s{_lorsEncodingType = a})

-- | Undocumented member.
lorsName :: Lens' ListObjectsResponse (Maybe BucketName)
lorsName = lens _lorsName (\ s a -> s{_lorsName = a})

-- | Undocumented member.
lorsMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsMarker = lens _lorsMarker (\ s a -> s{_lorsMarker = a})

-- | When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMaker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
lorsNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsNextMarker = lens _lorsNextMarker (\ s a -> s{_lorsNextMarker = a})

-- | Undocumented member.
lorsMaxKeys :: Lens' ListObjectsResponse (Maybe Int)
lorsMaxKeys = lens _lorsMaxKeys (\ s a -> s{_lorsMaxKeys = a})

-- | A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
lorsIsTruncated :: Lens' ListObjectsResponse (Maybe Bool)
lorsIsTruncated = lens _lorsIsTruncated (\ s a -> s{_lorsIsTruncated = a})

-- | Undocumented member.
lorsDelimiter :: Lens' ListObjectsResponse (Maybe Delimiter)
lorsDelimiter = lens _lorsDelimiter (\ s a -> s{_lorsDelimiter = a})

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListObjectsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\ s a -> s{_lorsResponseStatus = a})

instance NFData ListObjectsResponse where
