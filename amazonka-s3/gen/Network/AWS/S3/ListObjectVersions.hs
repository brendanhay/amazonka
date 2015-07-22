{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about all of the versions of objects in a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListObjectVersions.html>
module Network.AWS.S3.ListObjectVersions
    (
    -- * Request
      ListObjectVersions
    -- ** Request constructor
    , listObjectVersions
    -- ** Request lenses
    , lovrqKeyMarker
    , lovrqPrefix
    , lovrqEncodingType
    , lovrqVersionIdMarker
    , lovrqMaxKeys
    , lovrqDelimiter
    , lovrqBucket

    -- * Response
    , ListObjectVersionsResponse
    -- ** Response constructor
    , listObjectVersionsResponse
    -- ** Response lenses
    , lovrsNextVersionIdMarker
    , lovrsKeyMarker
    , lovrsPrefix
    , lovrsDeleteMarkers
    , lovrsEncodingType
    , lovrsCommonPrefixes
    , lovrsVersions
    , lovrsName
    , lovrsNextKeyMarker
    , lovrsVersionIdMarker
    , lovrsMaxKeys
    , lovrsIsTruncated
    , lovrsDelimiter
    , lovrsStatus
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
-- * 'lovrqKeyMarker'
--
-- * 'lovrqPrefix'
--
-- * 'lovrqEncodingType'
--
-- * 'lovrqVersionIdMarker'
--
-- * 'lovrqMaxKeys'
--
-- * 'lovrqDelimiter'
--
-- * 'lovrqBucket'
data ListObjectVersions = ListObjectVersions'
    { _lovrqKeyMarker       :: !(Maybe Text)
    , _lovrqPrefix          :: !(Maybe Text)
    , _lovrqEncodingType    :: !(Maybe EncodingType)
    , _lovrqVersionIdMarker :: !(Maybe Text)
    , _lovrqMaxKeys         :: !(Maybe Int)
    , _lovrqDelimiter       :: !(Maybe Char)
    , _lovrqBucket          :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjectVersions' smart constructor.
listObjectVersions :: BucketName -> ListObjectVersions
listObjectVersions pBucket =
    ListObjectVersions'
    { _lovrqKeyMarker = Nothing
    , _lovrqPrefix = Nothing
    , _lovrqEncodingType = Nothing
    , _lovrqVersionIdMarker = Nothing
    , _lovrqMaxKeys = Nothing
    , _lovrqDelimiter = Nothing
    , _lovrqBucket = pBucket
    }

-- | Specifies the key to start with when listing objects in a bucket.
lovrqKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lovrqKeyMarker = lens _lovrqKeyMarker (\ s a -> s{_lovrqKeyMarker = a});

-- | Limits the response to keys that begin with the specified prefix.
lovrqPrefix :: Lens' ListObjectVersions (Maybe Text)
lovrqPrefix = lens _lovrqPrefix (\ s a -> s{_lovrqPrefix = a});

-- | FIXME: Undocumented member.
lovrqEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lovrqEncodingType = lens _lovrqEncodingType (\ s a -> s{_lovrqEncodingType = a});

-- | Specifies the object version you want to start listing from.
lovrqVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lovrqVersionIdMarker = lens _lovrqVersionIdMarker (\ s a -> s{_lovrqVersionIdMarker = a});

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lovrqMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lovrqMaxKeys = lens _lovrqMaxKeys (\ s a -> s{_lovrqMaxKeys = a});

-- | A delimiter is a character you use to group keys.
lovrqDelimiter :: Lens' ListObjectVersions (Maybe Char)
lovrqDelimiter = lens _lovrqDelimiter (\ s a -> s{_lovrqDelimiter = a});

-- | FIXME: Undocumented member.
lovrqBucket :: Lens' ListObjectVersions BucketName
lovrqBucket = lens _lovrqBucket (\ s a -> s{_lovrqBucket = a});

instance AWSPager ListObjectVersions where
        page rq rs
          | stop (rs ^. lovrsIsTruncated) = Nothing
          | isNothing (rs ^. lovrsNextKeyMarker) &&
              isNothing (rs ^. lovrsNextVersionIdMarker)
            = Nothing
          | otherwise =
            Just $ rq &
              lovrqKeyMarker .~ rs ^. lovrsNextKeyMarker
              &
              lovrqVersionIdMarker .~
                rs ^. lovrsNextVersionIdMarker

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
          = mconcat ["/", toText _lovrqBucket]

instance ToQuery ListObjectVersions where
        toQuery ListObjectVersions'{..}
          = mconcat
              ["key-marker" =: _lovrqKeyMarker,
               "prefix" =: _lovrqPrefix,
               "encoding-type" =: _lovrqEncodingType,
               "version-id-marker" =: _lovrqVersionIdMarker,
               "max-keys" =: _lovrqMaxKeys,
               "delimiter" =: _lovrqDelimiter, "versions"]

-- | /See:/ 'listObjectVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lovrsNextVersionIdMarker'
--
-- * 'lovrsKeyMarker'
--
-- * 'lovrsPrefix'
--
-- * 'lovrsDeleteMarkers'
--
-- * 'lovrsEncodingType'
--
-- * 'lovrsCommonPrefixes'
--
-- * 'lovrsVersions'
--
-- * 'lovrsName'
--
-- * 'lovrsNextKeyMarker'
--
-- * 'lovrsVersionIdMarker'
--
-- * 'lovrsMaxKeys'
--
-- * 'lovrsIsTruncated'
--
-- * 'lovrsDelimiter'
--
-- * 'lovrsStatus'
data ListObjectVersionsResponse = ListObjectVersionsResponse'
    { _lovrsNextVersionIdMarker :: !(Maybe Text)
    , _lovrsKeyMarker           :: !(Maybe Text)
    , _lovrsPrefix              :: !(Maybe Text)
    , _lovrsDeleteMarkers       :: !(Maybe [DeleteMarkerEntry])
    , _lovrsEncodingType        :: !(Maybe EncodingType)
    , _lovrsCommonPrefixes      :: !(Maybe [CommonPrefix])
    , _lovrsVersions            :: !(Maybe [ObjectVersion])
    , _lovrsName                :: !(Maybe BucketName)
    , _lovrsNextKeyMarker       :: !(Maybe Text)
    , _lovrsVersionIdMarker     :: !(Maybe Text)
    , _lovrsMaxKeys             :: !(Maybe Int)
    , _lovrsIsTruncated         :: !(Maybe Bool)
    , _lovrsDelimiter           :: !(Maybe Char)
    , _lovrsStatus              :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjectVersionsResponse' smart constructor.
listObjectVersionsResponse :: Int -> ListObjectVersionsResponse
listObjectVersionsResponse pStatus =
    ListObjectVersionsResponse'
    { _lovrsNextVersionIdMarker = Nothing
    , _lovrsKeyMarker = Nothing
    , _lovrsPrefix = Nothing
    , _lovrsDeleteMarkers = Nothing
    , _lovrsEncodingType = Nothing
    , _lovrsCommonPrefixes = Nothing
    , _lovrsVersions = Nothing
    , _lovrsName = Nothing
    , _lovrsNextKeyMarker = Nothing
    , _lovrsVersionIdMarker = Nothing
    , _lovrsMaxKeys = Nothing
    , _lovrsIsTruncated = Nothing
    , _lovrsDelimiter = Nothing
    , _lovrsStatus = pStatus
    }

-- | Use this value for the next version id marker parameter in a subsequent
-- request.
lovrsNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextVersionIdMarker = lens _lovrsNextVersionIdMarker (\ s a -> s{_lovrsNextVersionIdMarker = a});

-- | Marks the last Key returned in a truncated response.
lovrsKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsKeyMarker = lens _lovrsKeyMarker (\ s a -> s{_lovrsKeyMarker = a});

-- | FIXME: Undocumented member.
lovrsPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsPrefix = lens _lovrsPrefix (\ s a -> s{_lovrsPrefix = a});

-- | FIXME: Undocumented member.
lovrsDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lovrsDeleteMarkers = lens _lovrsDeleteMarkers (\ s a -> s{_lovrsDeleteMarkers = a}) . _Default;

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrsEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lovrsEncodingType = lens _lovrsEncodingType (\ s a -> s{_lovrsEncodingType = a});

-- | FIXME: Undocumented member.
lovrsCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lovrsCommonPrefixes = lens _lovrsCommonPrefixes (\ s a -> s{_lovrsCommonPrefixes = a}) . _Default;

-- | FIXME: Undocumented member.
lovrsVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lovrsVersions = lens _lovrsVersions (\ s a -> s{_lovrsVersions = a}) . _Default;

-- | FIXME: Undocumented member.
lovrsName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lovrsName = lens _lovrsName (\ s a -> s{_lovrsName = a});

-- | Use this value for the key marker request parameter in a subsequent
-- request.
lovrsNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsNextKeyMarker = lens _lovrsNextKeyMarker (\ s a -> s{_lovrsNextKeyMarker = a});

-- | FIXME: Undocumented member.
lovrsVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lovrsVersionIdMarker = lens _lovrsVersionIdMarker (\ s a -> s{_lovrsVersionIdMarker = a});

-- | FIXME: Undocumented member.
lovrsMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lovrsMaxKeys = lens _lovrsMaxKeys (\ s a -> s{_lovrsMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria. If your results were
-- truncated, you can make a follow-up paginated request using the
-- NextKeyMarker and NextVersionIdMarker response parameters as a starting
-- place in another request to return the rest of the results.
lovrsIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lovrsIsTruncated = lens _lovrsIsTruncated (\ s a -> s{_lovrsIsTruncated = a});

-- | FIXME: Undocumented member.
lovrsDelimiter :: Lens' ListObjectVersionsResponse (Maybe Char)
lovrsDelimiter = lens _lovrsDelimiter (\ s a -> s{_lovrsDelimiter = a});

-- | FIXME: Undocumented member.
lovrsStatus :: Lens' ListObjectVersionsResponse Int
lovrsStatus = lens _lovrsStatus (\ s a -> s{_lovrsStatus = a});
