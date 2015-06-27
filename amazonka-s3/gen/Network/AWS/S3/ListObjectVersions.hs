{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.ListObjectVersions
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

-- | Returns metadata about all of the versions of objects in a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListObjectVersions.html>
module Network.AWS.S3.ListObjectVersions
    (
    -- * Request
      ListObjectVersions
    -- ** Request constructor
    , listObjectVersions
    -- ** Request lenses
    , lovKeyMarker
    , lovPrefix
    , lovEncodingType
    , lovVersionIdMarker
    , lovMaxKeys
    , lovDelimiter
    , lovBucket

    -- * Response
    , ListObjectVersionsResponse
    -- ** Response constructor
    , listObjectVersionsResponse
    -- ** Response lenses
    , lovrNextVersionIdMarker
    , lovrKeyMarker
    , lovrPrefix
    , lovrDeleteMarkers
    , lovrEncodingType
    , lovrCommonPrefixes
    , lovrVersions
    , lovrName
    , lovrNextKeyMarker
    , lovrVersionIdMarker
    , lovrMaxKeys
    , lovrIsTruncated
    , lovrDelimiter
    , lovrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'listObjectVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovKeyMarker'
--
-- * 'lovPrefix'
--
-- * 'lovEncodingType'
--
-- * 'lovVersionIdMarker'
--
-- * 'lovMaxKeys'
--
-- * 'lovDelimiter'
--
-- * 'lovBucket'
data ListObjectVersions = ListObjectVersions'
    { _lovKeyMarker       :: Maybe Text
    , _lovPrefix          :: Maybe Text
    , _lovEncodingType    :: Maybe EncodingType
    , _lovVersionIdMarker :: Maybe Text
    , _lovMaxKeys         :: Maybe Int
    , _lovDelimiter       :: Maybe Char
    , _lovBucket          :: BucketName
    } deriving (Eq,Read,Show)

-- | 'ListObjectVersions' smart constructor.
listObjectVersions :: BucketName -> ListObjectVersions
listObjectVersions pBucket =
    ListObjectVersions'
    { _lovKeyMarker = Nothing
    , _lovPrefix = Nothing
    , _lovEncodingType = Nothing
    , _lovVersionIdMarker = Nothing
    , _lovMaxKeys = Nothing
    , _lovDelimiter = Nothing
    , _lovBucket = pBucket
    }

-- | Specifies the key to start with when listing objects in a bucket.
lovKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovKeyMarker = lens _lovKeyMarker (\ s a -> s{_lovKeyMarker = a});

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectVersions (Maybe Text)
lovPrefix = lens _lovPrefix (\ s a -> s{_lovPrefix = a});

-- | FIXME: Undocumented member.
lovEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lovEncodingType = lens _lovEncodingType (\ s a -> s{_lovEncodingType = a});

-- | Specifies the object version you want to start listing from.
lovVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovVersionIdMarker = lens _lovVersionIdMarker (\ s a -> s{_lovVersionIdMarker = a});

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lovMaxKeys = lens _lovMaxKeys (\ s a -> s{_lovMaxKeys = a});

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectVersions (Maybe Char)
lovDelimiter = lens _lovDelimiter (\ s a -> s{_lovDelimiter = a});

-- | FIXME: Undocumented member.
lovBucket :: Lens' ListObjectVersions BucketName
lovBucket = lens _lovBucket (\ s a -> s{_lovBucket = a});

instance AWSPager ListObjectVersions where
        page rq rs
          | stop (rs ^. lovrIsTruncated) = Nothing
          | isNothing (rs ^. lovrNextKeyMarker) &&
              isNothing (rs ^. lovrNextVersionIdMarker)
            = Nothing
          | otherwise =
            Just $ rq & lovKeyMarker .~ rs ^. lovrNextKeyMarker &
              lovVersionIdMarker .~ rs ^. lovrNextVersionIdMarker

instance AWSRequest ListObjectVersions where
        type Sv ListObjectVersions = S3
        type Rs ListObjectVersions =
             ListObjectVersionsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListObjectVersionsResponse' <$>
                   (x .@? "NextVersionIdMarker") <*> (x .@? "KeyMarker")
                     <*> (x .@? "Prefix")
                     <*> (may (parseXMLList "DeleteMarker") x)
                     <*> (x .@? "EncodingType")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (may (parseXMLList "Version") x)
                     <*> (x .@? "Name")
                     <*> (x .@? "NextKeyMarker")
                     <*> (x .@? "VersionIdMarker")
                     <*> (x .@? "MaxKeys")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListObjectVersions where
        toHeaders = const mempty

instance ToPath ListObjectVersions where
        toPath ListObjectVersions'{..}
          = mconcat ["/", toText _lovBucket]

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovrNextVersionIdMarker'
--
-- * 'lovrKeyMarker'
--
-- * 'lovrPrefix'
--
-- * 'lovrDeleteMarkers'
--
-- * 'lovrEncodingType'
--
-- * 'lovrCommonPrefixes'
--
-- * 'lovrVersions'
--
-- * 'lovrName'
--
-- * 'lovrNextKeyMarker'
--
-- * 'lovrVersionIdMarker'
--
-- * 'lovrMaxKeys'
--
-- * 'lovrIsTruncated'
--
-- * 'lovrDelimiter'
--
-- * 'lovrStatus'
data ListObjectVersionsResponse = ListObjectVersionsResponse'
    { _lovrNextVersionIdMarker :: Maybe Text
    , _lovrKeyMarker           :: Maybe Text
    , _lovrPrefix              :: Maybe Text
    , _lovrDeleteMarkers       :: Maybe [DeleteMarkerEntry]
    , _lovrEncodingType        :: Maybe EncodingType
    , _lovrCommonPrefixes      :: Maybe [CommonPrefix]
    , _lovrVersions            :: Maybe [ObjectVersion]
    , _lovrName                :: Maybe BucketName
    , _lovrNextKeyMarker       :: Maybe Text
    , _lovrVersionIdMarker     :: Maybe Text
    , _lovrMaxKeys             :: Maybe Int
    , _lovrIsTruncated         :: Maybe Bool
    , _lovrDelimiter           :: Maybe Char
    , _lovrStatus              :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListObjectVersionsResponse' smart constructor.
listObjectVersionsResponse :: Int -> ListObjectVersionsResponse
listObjectVersionsResponse pStatus =
    ListObjectVersionsResponse'
    { _lovrNextVersionIdMarker = Nothing
    , _lovrKeyMarker = Nothing
    , _lovrPrefix = Nothing
    , _lovrDeleteMarkers = Nothing
    , _lovrEncodingType = Nothing
    , _lovrCommonPrefixes = Nothing
    , _lovrVersions = Nothing
    , _lovrName = Nothing
    , _lovrNextKeyMarker = Nothing
    , _lovrVersionIdMarker = Nothing
    , _lovrMaxKeys = Nothing
    , _lovrIsTruncated = Nothing
    , _lovrDelimiter = Nothing
    , _lovrStatus = pStatus
    }

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovrNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextVersionIdMarker = lens _lovrNextVersionIdMarker (\ s a -> s{_lovrNextVersionIdMarker = a});

-- | Marks the last Key returned in a truncated response.
lovrKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrKeyMarker = lens _lovrKeyMarker (\ s a -> s{_lovrKeyMarker = a});

-- | FIXME: Undocumented member.
lovrPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrPrefix = lens _lovrPrefix (\ s a -> s{_lovrPrefix = a});

-- | FIXME: Undocumented member.
lovrDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrDeleteMarkers = lens _lovrDeleteMarkers (\ s a -> s{_lovrDeleteMarkers = a}) . _Default;

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovrEncodingType = lens _lovrEncodingType (\ s a -> s{_lovrEncodingType = a});

-- | FIXME: Undocumented member.
lovrCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrCommonPrefixes = lens _lovrCommonPrefixes (\ s a -> s{_lovrCommonPrefixes = a}) . _Default;

-- | FIXME: Undocumented member.
lovrVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrVersions = lens _lovrVersions (\ s a -> s{_lovrVersions = a}) . _Default;

-- | FIXME: Undocumented member.
lovrName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovrName = lens _lovrName (\ s a -> s{_lovrName = a});

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovrNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrNextKeyMarker = lens _lovrNextKeyMarker (\ s a -> s{_lovrNextKeyMarker = a});

-- | FIXME: Undocumented member.
lovrVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrVersionIdMarker = lens _lovrVersionIdMarker (\ s a -> s{_lovrVersionIdMarker = a});

-- | FIXME: Undocumented member.
lovrMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lovrMaxKeys = lens _lovrMaxKeys (\ s a -> s{_lovrMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria. If your results were
-- truncated, you can make a follow-up paginated request using the
-- NextKeyMarker and NextVersionIdMarker response parameters as a starting
-- place in another request to return the rest of the results.
lovrIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lovrIsTruncated = lens _lovrIsTruncated (\ s a -> s{_lovrIsTruncated = a});

-- | FIXME: Undocumented member.
lovrDelimiter :: Lens' ListObjectVersionsResponse (Maybe Char)
lovrDelimiter = lens _lovrDelimiter (\ s a -> s{_lovrDelimiter = a});

-- | FIXME: Undocumented member.
lovrStatus :: Lens' ListObjectVersionsResponse Int
lovrStatus = lens _lovrStatus (\ s a -> s{_lovrStatus = a});
