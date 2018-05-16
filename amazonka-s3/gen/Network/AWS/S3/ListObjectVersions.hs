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
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    , lKeyMarker
    , lPrefix
    , lEncodingType
    , lVersionIdMarker
    , lMaxKeys
    , lDelimiter
    , lBucket

    -- * Destructuring the Response
    , listObjectVersionsResponse
    , ListObjectVersionsResponse
    -- * Response Lenses
    , lrsNextVersionIdMarker
    , lrsKeyMarker
    , lrsDeleteMarkers
    , lrsPrefix
    , lrsCommonPrefixes
    , lrsEncodingType
    , lrsVersions
    , lrsName
    , lrsNextKeyMarker
    , lrsVersionIdMarker
    , lrsMaxKeys
    , lrsIsTruncated
    , lrsDelimiter
    , lrsResponseStatus
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
  { _lKeyMarker       :: !(Maybe Text)
  , _lPrefix          :: !(Maybe Text)
  , _lEncodingType    :: !(Maybe EncodingType)
  , _lVersionIdMarker :: !(Maybe Text)
  , _lMaxKeys         :: !(Maybe Int)
  , _lDelimiter       :: !(Maybe Delimiter)
  , _lBucket          :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lKeyMarker' - Specifies the key to start with when listing objects in a bucket.
--
-- * 'lPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lEncodingType' - Undocumented member.
--
-- * 'lVersionIdMarker' - Specifies the object version you want to start listing from.
--
-- * 'lMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lBucket' - Undocumented member.
listObjectVersions
    :: BucketName -- ^ 'lBucket'
    -> ListObjectVersions
listObjectVersions pBucket_ =
  ListObjectVersions'
    { _lKeyMarker = Nothing
    , _lPrefix = Nothing
    , _lEncodingType = Nothing
    , _lVersionIdMarker = Nothing
    , _lMaxKeys = Nothing
    , _lDelimiter = Nothing
    , _lBucket = pBucket_
    }


-- | Specifies the key to start with when listing objects in a bucket.
lKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lKeyMarker = lens _lKeyMarker (\ s a -> s{_lKeyMarker = a})

-- | Limits the response to keys that begin with the specified prefix.
lPrefix :: Lens' ListObjectVersions (Maybe Text)
lPrefix = lens _lPrefix (\ s a -> s{_lPrefix = a})

-- | Undocumented member.
lEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lEncodingType = lens _lEncodingType (\ s a -> s{_lEncodingType = a})

-- | Specifies the object version you want to start listing from.
lVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lVersionIdMarker = lens _lVersionIdMarker (\ s a -> s{_lVersionIdMarker = a})

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lMaxKeys = lens _lMaxKeys (\ s a -> s{_lMaxKeys = a})

-- | A delimiter is a character you use to group keys.
lDelimiter :: Lens' ListObjectVersions (Maybe Delimiter)
lDelimiter = lens _lDelimiter (\ s a -> s{_lDelimiter = a})

-- | Undocumented member.
lBucket :: Lens' ListObjectVersions BucketName
lBucket = lens _lBucket (\ s a -> s{_lBucket = a})

instance AWSPager ListObjectVersions where
        page rq rs
          | stop (rs ^. lrsIsTruncated) = Nothing
          | isNothing (rs ^. lrsNextKeyMarker) &&
              isNothing (rs ^. lrsNextVersionIdMarker)
            = Nothing
          | otherwise =
            Just $ rq & lKeyMarker .~ rs ^. lrsNextKeyMarker &
              lVersionIdMarker .~ rs ^. lrsNextVersionIdMarker

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
          = mconcat ["/", toBS _lBucket]

instance ToQuery ListObjectVersions where
        toQuery ListObjectVersions'{..}
          = mconcat
              ["key-marker" =: _lKeyMarker, "prefix" =: _lPrefix,
               "encoding-type" =: _lEncodingType,
               "version-id-marker" =: _lVersionIdMarker,
               "max-keys" =: _lMaxKeys, "delimiter" =: _lDelimiter,
               "versions"]

-- | /See:/ 'listObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { _lrsNextVersionIdMarker :: !(Maybe Text)
  , _lrsKeyMarker           :: !(Maybe Text)
  , _lrsDeleteMarkers       :: !(Maybe [DeleteMarkerEntry])
  , _lrsPrefix              :: !(Maybe Text)
  , _lrsCommonPrefixes      :: !(Maybe [CommonPrefix])
  , _lrsEncodingType        :: !(Maybe EncodingType)
  , _lrsVersions            :: !(Maybe [ObjectVersion])
  , _lrsName                :: !(Maybe BucketName)
  , _lrsNextKeyMarker       :: !(Maybe Text)
  , _lrsVersionIdMarker     :: !(Maybe Text)
  , _lrsMaxKeys             :: !(Maybe Int)
  , _lrsIsTruncated         :: !(Maybe Bool)
  , _lrsDelimiter           :: !(Maybe Delimiter)
  , _lrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextVersionIdMarker' - Use this value for the next version id marker parameter in a subsequent request.
--
-- * 'lrsKeyMarker' - Marks the last Key returned in a truncated response.
--
-- * 'lrsDeleteMarkers' - Undocumented member.
--
-- * 'lrsPrefix' - Undocumented member.
--
-- * 'lrsCommonPrefixes' - Undocumented member.
--
-- * 'lrsEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lrsVersions' - Undocumented member.
--
-- * 'lrsName' - Undocumented member.
--
-- * 'lrsNextKeyMarker' - Use this value for the key marker request parameter in a subsequent request.
--
-- * 'lrsVersionIdMarker' - Undocumented member.
--
-- * 'lrsMaxKeys' - Undocumented member.
--
-- * 'lrsIsTruncated' - A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- * 'lrsDelimiter' - Undocumented member.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listObjectVersionsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListObjectVersionsResponse
listObjectVersionsResponse pResponseStatus_ =
  ListObjectVersionsResponse'
    { _lrsNextVersionIdMarker = Nothing
    , _lrsKeyMarker = Nothing
    , _lrsDeleteMarkers = Nothing
    , _lrsPrefix = Nothing
    , _lrsCommonPrefixes = Nothing
    , _lrsEncodingType = Nothing
    , _lrsVersions = Nothing
    , _lrsName = Nothing
    , _lrsNextKeyMarker = Nothing
    , _lrsVersionIdMarker = Nothing
    , _lrsMaxKeys = Nothing
    , _lrsIsTruncated = Nothing
    , _lrsDelimiter = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | Use this value for the next version id marker parameter in a subsequent request.
lrsNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsNextVersionIdMarker = lens _lrsNextVersionIdMarker (\ s a -> s{_lrsNextVersionIdMarker = a})

-- | Marks the last Key returned in a truncated response.
lrsKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsKeyMarker = lens _lrsKeyMarker (\ s a -> s{_lrsKeyMarker = a})

-- | Undocumented member.
lrsDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lrsDeleteMarkers = lens _lrsDeleteMarkers (\ s a -> s{_lrsDeleteMarkers = a}) . _Default . _Coerce

-- | Undocumented member.
lrsPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsPrefix = lens _lrsPrefix (\ s a -> s{_lrsPrefix = a})

-- | Undocumented member.
lrsCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lrsCommonPrefixes = lens _lrsCommonPrefixes (\ s a -> s{_lrsCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lrsEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lrsEncodingType = lens _lrsEncodingType (\ s a -> s{_lrsEncodingType = a})

-- | Undocumented member.
lrsVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lrsVersions = lens _lrsVersions (\ s a -> s{_lrsVersions = a}) . _Default . _Coerce

-- | Undocumented member.
lrsName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lrsName = lens _lrsName (\ s a -> s{_lrsName = a})

-- | Use this value for the key marker request parameter in a subsequent request.
lrsNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsNextKeyMarker = lens _lrsNextKeyMarker (\ s a -> s{_lrsNextKeyMarker = a})

-- | Undocumented member.
lrsVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsVersionIdMarker = lens _lrsVersionIdMarker (\ s a -> s{_lrsVersionIdMarker = a})

-- | Undocumented member.
lrsMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lrsMaxKeys = lens _lrsMaxKeys (\ s a -> s{_lrsMaxKeys = a})

-- | A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
lrsIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lrsIsTruncated = lens _lrsIsTruncated (\ s a -> s{_lrsIsTruncated = a})

-- | Undocumented member.
lrsDelimiter :: Lens' ListObjectVersionsResponse (Maybe Delimiter)
lrsDelimiter = lens _lrsDelimiter (\ s a -> s{_lrsDelimiter = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListObjectVersionsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListObjectVersionsResponse where
