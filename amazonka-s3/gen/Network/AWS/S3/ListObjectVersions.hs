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
-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about all of the versions of objects in a bucket.
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectVersions
    (
    -- * Creating a Request
      listObjectVersions
    , ListObjectVersions
    -- * Request Lenses
    , lovKeyMarker
    , lovPrefix
    , lovEncodingType
    , lovVersionIdMarker
    , lovMaxKeys
    , lovDelimiter
    , lovBucket

    -- * Destructuring the Response
    , listObjectVersionsResponse
    , ListObjectVersionsResponse
    -- * Response Lenses
    , lovrsNextVersionIdMarker
    , lovrsKeyMarker
    , lovrsDeleteMarkers
    , lovrsPrefix
    , lovrsCommonPrefixes
    , lovrsEncodingType
    , lovrsVersions
    , lovrsName
    , lovrsNextKeyMarker
    , lovrsVersionIdMarker
    , lovrsMaxKeys
    , lovrsIsTruncated
    , lovrsDelimiter
    , lovrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { _lovKeyMarker       :: !(Maybe Text)
  , _lovPrefix          :: !(Maybe Text)
  , _lovEncodingType    :: !(Maybe EncodingType)
  , _lovVersionIdMarker :: !(Maybe Text)
  , _lovMaxKeys         :: !(Maybe Int)
  , _lovDelimiter       :: !(Maybe Delimiter)
  , _lovBucket          :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovKeyMarker' - Specifies the key to start with when listing objects in a bucket.
--
-- * 'lovPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lovEncodingType' - Undocumented member.
--
-- * 'lovVersionIdMarker' - Specifies the object version you want to start listing from.
--
-- * 'lovMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lovDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lovBucket' - Undocumented member.
listObjectVersions
    :: BucketName -- ^ 'lovBucket'
    -> ListObjectVersions
listObjectVersions pBucket_ =
  ListObjectVersions'
  { _lovKeyMarker = Nothing
  , _lovPrefix = Nothing
  , _lovEncodingType = Nothing
  , _lovVersionIdMarker = Nothing
  , _lovMaxKeys = Nothing
  , _lovDelimiter = Nothing
  , _lovBucket = pBucket_
  }


-- | Specifies the key to start with when listing objects in a bucket.
lovKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovKeyMarker = lens _lovKeyMarker (\ s a -> s{_lovKeyMarker = a});

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectVersions (Maybe Text)
lovPrefix = lens _lovPrefix (\ s a -> s{_lovPrefix = a});

-- | Undocumented member.
lovEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lovEncodingType = lens _lovEncodingType (\ s a -> s{_lovEncodingType = a});

-- | Specifies the object version you want to start listing from.
lovVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovVersionIdMarker = lens _lovVersionIdMarker (\ s a -> s{_lovVersionIdMarker = a});

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lovMaxKeys = lens _lovMaxKeys (\ s a -> s{_lovMaxKeys = a});

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectVersions (Maybe Delimiter)
lovDelimiter = lens _lovDelimiter (\ s a -> s{_lovDelimiter = a});

-- | Undocumented member.
lovBucket :: Lens' ListObjectVersions BucketName
lovBucket = lens _lovBucket (\ s a -> s{_lovBucket = a});

instance AWSPager ListObjectVersions where
        page rq rs
          | stop (rs ^. lovrsIsTruncated) = Nothing
          | isNothing (rs ^. lovrsNextKeyMarker) &&
              isNothing (rs ^. lovrsNextVersionIdMarker)
            = Nothing
          | otherwise =
            Just $ rq & lovKeyMarker .~ rs ^. lovrsNextKeyMarker
              &
              lovVersionIdMarker .~ rs ^. lovrsNextVersionIdMarker

instance AWSRequest ListObjectVersions where
        type Rs ListObjectVersions =
             ListObjectVersionsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListObjectVersionsResponse' <$>
                   (x .@? "NextVersionIdMarker") <*> (x .@? "KeyMarker")
                     <*> (may (parseXMLList "DeleteMarker") x)
                     <*> (x .@? "Prefix")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "EncodingType")
                     <*> (may (parseXMLList "Version") x)
                     <*> (x .@? "Name")
                     <*> (x .@? "NextKeyMarker")
                     <*> (x .@? "VersionIdMarker")
                     <*> (x .@? "MaxKeys")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectVersions where

instance NFData ListObjectVersions where

instance ToHeaders ListObjectVersions where
        toHeaders = const mempty

instance ToPath ListObjectVersions where
        toPath ListObjectVersions'{..}
          = mconcat ["/", toBS _lovBucket]

instance ToQuery ListObjectVersions where
        toQuery ListObjectVersions'{..}
          = mconcat
              ["key-marker" =: _lovKeyMarker,
               "prefix" =: _lovPrefix,
               "encoding-type" =: _lovEncodingType,
               "version-id-marker" =: _lovVersionIdMarker,
               "max-keys" =: _lovMaxKeys,
               "delimiter" =: _lovDelimiter, "versions"]

-- | /See:/ 'listObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { _lovrsNextVersionIdMarker :: !(Maybe Text)
  , _lovrsKeyMarker           :: !(Maybe Text)
  , _lovrsDeleteMarkers       :: !(Maybe [DeleteMarkerEntry])
  , _lovrsPrefix              :: !(Maybe Text)
  , _lovrsCommonPrefixes      :: !(Maybe [CommonPrefix])
  , _lovrsEncodingType        :: !(Maybe EncodingType)
  , _lovrsVersions            :: !(Maybe [ObjectVersion])
  , _lovrsName                :: !(Maybe BucketName)
  , _lovrsNextKeyMarker       :: !(Maybe Text)
  , _lovrsVersionIdMarker     :: !(Maybe Text)
  , _lovrsMaxKeys             :: !(Maybe Int)
  , _lovrsIsTruncated         :: !(Maybe Bool)
  , _lovrsDelimiter           :: !(Maybe Delimiter)
  , _lovrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovrsNextVersionIdMarker' - Use this value for the next version id marker parameter in a subsequent request.
--
-- * 'lovrsKeyMarker' - Marks the last Key returned in a truncated response.
--
-- * 'lovrsDeleteMarkers' - Undocumented member.
--
-- * 'lovrsPrefix' - Undocumented member.
--
-- * 'lovrsCommonPrefixes' - Undocumented member.
--
-- * 'lovrsEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lovrsVersions' - Undocumented member.
--
-- * 'lovrsName' - Undocumented member.
--
-- * 'lovrsNextKeyMarker' - Use this value for the key marker request parameter in a subsequent request.
--
-- * 'lovrsVersionIdMarker' - Undocumented member.
--
-- * 'lovrsMaxKeys' - Undocumented member.
--
-- * 'lovrsIsTruncated' - A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- * 'lovrsDelimiter' - Undocumented member.
--
-- * 'lovrsResponseStatus' - -- | The response status code.
listObjectVersionsResponse
    :: Int -- ^ 'lovrsResponseStatus'
    -> ListObjectVersionsResponse
listObjectVersionsResponse pResponseStatus_ =
  ListObjectVersionsResponse'
  { _lovrsNextVersionIdMarker = Nothing
  , _lovrsKeyMarker = Nothing
  , _lovrsDeleteMarkers = Nothing
  , _lovrsPrefix = Nothing
  , _lovrsCommonPrefixes = Nothing
  , _lovrsEncodingType = Nothing
  , _lovrsVersions = Nothing
  , _lovrsName = Nothing
  , _lovrsNextKeyMarker = Nothing
  , _lovrsVersionIdMarker = Nothing
  , _lovrsMaxKeys = Nothing
  , _lovrsIsTruncated = Nothing
  , _lovrsDelimiter = Nothing
  , _lovrsResponseStatus = pResponseStatus_
  }


-- | Use this value for the next version id marker parameter in a subsequent request.
lovrsNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextVersionIdMarker = lens _lovrsNextVersionIdMarker (\ s a -> s{_lovrsNextVersionIdMarker = a});

-- | Marks the last Key returned in a truncated response.
lovrsKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsKeyMarker = lens _lovrsKeyMarker (\ s a -> s{_lovrsKeyMarker = a});

-- | Undocumented member.
lovrsDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrsDeleteMarkers = lens _lovrsDeleteMarkers (\ s a -> s{_lovrsDeleteMarkers = a}) . _Default . _Coerce;

-- | Undocumented member.
lovrsPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsPrefix = lens _lovrsPrefix (\ s a -> s{_lovrsPrefix = a});

-- | Undocumented member.
lovrsCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrsCommonPrefixes = lens _lovrsCommonPrefixes (\ s a -> s{_lovrsCommonPrefixes = a}) . _Default . _Coerce;

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrsEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovrsEncodingType = lens _lovrsEncodingType (\ s a -> s{_lovrsEncodingType = a});

-- | Undocumented member.
lovrsVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrsVersions = lens _lovrsVersions (\ s a -> s{_lovrsVersions = a}) . _Default . _Coerce;

-- | Undocumented member.
lovrsName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovrsName = lens _lovrsName (\ s a -> s{_lovrsName = a});

-- | Use this value for the key marker request parameter in a subsequent request.
lovrsNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextKeyMarker = lens _lovrsNextKeyMarker (\ s a -> s{_lovrsNextKeyMarker = a});

-- | Undocumented member.
lovrsVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsVersionIdMarker = lens _lovrsVersionIdMarker (\ s a -> s{_lovrsVersionIdMarker = a});

-- | Undocumented member.
lovrsMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lovrsMaxKeys = lens _lovrsMaxKeys (\ s a -> s{_lovrsMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
lovrsIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lovrsIsTruncated = lens _lovrsIsTruncated (\ s a -> s{_lovrsIsTruncated = a});

-- | Undocumented member.
lovrsDelimiter :: Lens' ListObjectVersionsResponse (Maybe Delimiter)
lovrsDelimiter = lens _lovrsDelimiter (\ s a -> s{_lovrsDelimiter = a});

-- | -- | The response status code.
lovrsResponseStatus :: Lens' ListObjectVersionsResponse Int
lovrsResponseStatus = lens _lovrsResponseStatus (\ s a -> s{_lovrsResponseStatus = a});

instance NFData ListObjectVersionsResponse where
