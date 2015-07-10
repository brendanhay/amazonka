{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListObjects.html>
module Network.AWS.S3.ListObjects
    (
    -- * Request
      ListObjects
    -- ** Request constructor
    , listObjects
    -- ** Request lenses
    , loPrefix
    , loEncodingType
    , loMarker
    , loMaxKeys
    , loDelimiter
    , loBucket

    -- * Response
    , ListObjectsResponse
    -- ** Response constructor
    , listObjectsResponse
    -- ** Response lenses
    , lorContents
    , lorPrefix
    , lorEncodingType
    , lorCommonPrefixes
    , lorName
    , lorMarker
    , lorNextMarker
    , lorMaxKeys
    , lorIsTruncated
    , lorDelimiter
    , lorStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'listObjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loPrefix'
--
-- * 'loEncodingType'
--
-- * 'loMarker'
--
-- * 'loMaxKeys'
--
-- * 'loDelimiter'
--
-- * 'loBucket'
data ListObjects = ListObjects'
    { _loPrefix       :: !(Maybe Text)
    , _loEncodingType :: !(Maybe EncodingType)
    , _loMarker       :: !(Maybe Text)
    , _loMaxKeys      :: !(Maybe Int)
    , _loDelimiter    :: !(Maybe Char)
    , _loBucket       :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjects' smart constructor.
listObjects :: BucketName -> ListObjects
listObjects pBucket =
    ListObjects'
    { _loPrefix = Nothing
    , _loEncodingType = Nothing
    , _loMarker = Nothing
    , _loMaxKeys = Nothing
    , _loDelimiter = Nothing
    , _loBucket = pBucket
    }

-- | Limits the response to keys that begin with the specified prefix.
loPrefix :: Lens' ListObjects (Maybe Text)
loPrefix = lens _loPrefix (\ s a -> s{_loPrefix = a});

-- | FIXME: Undocumented member.
loEncodingType :: Lens' ListObjects (Maybe EncodingType)
loEncodingType = lens _loEncodingType (\ s a -> s{_loEncodingType = a});

-- | Specifies the key to start with when listing objects in a bucket.
loMarker :: Lens' ListObjects (Maybe Text)
loMarker = lens _loMarker (\ s a -> s{_loMarker = a});

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
loMaxKeys :: Lens' ListObjects (Maybe Int)
loMaxKeys = lens _loMaxKeys (\ s a -> s{_loMaxKeys = a});

-- | A delimiter is a character you use to group keys.
loDelimiter :: Lens' ListObjects (Maybe Char)
loDelimiter = lens _loDelimiter (\ s a -> s{_loDelimiter = a});

-- | FIXME: Undocumented member.
loBucket :: Lens' ListObjects BucketName
loBucket = lens _loBucket (\ s a -> s{_loBucket = a});

instance AWSPager ListObjects where
        page rq rs
          | stop (rs ^. lorIsTruncated) = Nothing
          | isNothing
              (rs ^.
                 choice (^. lorNextMarker)
                   (^? (lorContents . _last . objKey)))
            = Nothing
          | otherwise =
            Just $ rq &
              loMarker .~
                rs ^.
                  choice (^. lorNextMarker)
                    (^? (lorContents . _last . objKey))

instance AWSRequest ListObjects where
        type Sv ListObjects = S3
        type Rs ListObjects = ListObjectsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListObjectsResponse' <$>
                   (may (parseXMLList "Contents") x) <*>
                     (x .@? "Prefix")
                     <*> (x .@? "EncodingType")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "Name")
                     <*> (x .@? "Marker")
                     <*> (x .@? "NextMarker")
                     <*> (x .@? "MaxKeys")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListObjects where
        toHeaders = const mempty

instance ToPath ListObjects where
        toPath ListObjects'{..}
          = mconcat ["/", toText _loBucket]

instance ToQuery ListObjects where
        toQuery ListObjects'{..}
          = mconcat
              ["prefix" =: _loPrefix,
               "encoding-type" =: _loEncodingType,
               "marker" =: _loMarker, "max-keys" =: _loMaxKeys,
               "delimiter" =: _loDelimiter]

-- | /See:/ 'listObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorContents'
--
-- * 'lorPrefix'
--
-- * 'lorEncodingType'
--
-- * 'lorCommonPrefixes'
--
-- * 'lorName'
--
-- * 'lorMarker'
--
-- * 'lorNextMarker'
--
-- * 'lorMaxKeys'
--
-- * 'lorIsTruncated'
--
-- * 'lorDelimiter'
--
-- * 'lorStatus'
data ListObjectsResponse = ListObjectsResponse'
    { _lorContents       :: !(Maybe [Object])
    , _lorPrefix         :: !(Maybe Text)
    , _lorEncodingType   :: !(Maybe EncodingType)
    , _lorCommonPrefixes :: !(Maybe [CommonPrefix])
    , _lorName           :: !(Maybe BucketName)
    , _lorMarker         :: !(Maybe Text)
    , _lorNextMarker     :: !(Maybe Text)
    , _lorMaxKeys        :: !(Maybe Int)
    , _lorIsTruncated    :: !(Maybe Bool)
    , _lorDelimiter      :: !(Maybe Char)
    , _lorStatus         :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjectsResponse' smart constructor.
listObjectsResponse :: Int -> ListObjectsResponse
listObjectsResponse pStatus =
    ListObjectsResponse'
    { _lorContents = Nothing
    , _lorPrefix = Nothing
    , _lorEncodingType = Nothing
    , _lorCommonPrefixes = Nothing
    , _lorName = Nothing
    , _lorMarker = Nothing
    , _lorNextMarker = Nothing
    , _lorMaxKeys = Nothing
    , _lorIsTruncated = Nothing
    , _lorDelimiter = Nothing
    , _lorStatus = pStatus
    }

-- | FIXME: Undocumented member.
lorContents :: Lens' ListObjectsResponse [Object]
lorContents = lens _lorContents (\ s a -> s{_lorContents = a}) . _Default;

-- | FIXME: Undocumented member.
lorPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorPrefix = lens _lorPrefix (\ s a -> s{_lorPrefix = a});

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
lorEncodingType = lens _lorEncodingType (\ s a -> s{_lorEncodingType = a});

-- | FIXME: Undocumented member.
lorCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorCommonPrefixes = lens _lorCommonPrefixes (\ s a -> s{_lorCommonPrefixes = a}) . _Default;

-- | FIXME: Undocumented member.
lorName :: Lens' ListObjectsResponse (Maybe BucketName)
lorName = lens _lorName (\ s a -> s{_lorName = a});

-- | FIXME: Undocumented member.
lorMarker :: Lens' ListObjectsResponse (Maybe Text)
lorMarker = lens _lorMarker (\ s a -> s{_lorMarker = a});

-- | When response is truncated (the IsTruncated element value in the
-- response is true), you can use the key name in this field as marker in
-- the subsequent request to get next set of objects. Amazon S3 lists
-- objects in alphabetical order Note: This element is returned only if you
-- have delimiter request parameter specified. If response does not include
-- the NextMaker and it is truncated, you can use the value of the last Key
-- in the response as the marker in the subsequent request to get the next
-- set of object keys.
lorNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorNextMarker = lens _lorNextMarker (\ s a -> s{_lorNextMarker = a});

-- | FIXME: Undocumented member.
lorMaxKeys :: Lens' ListObjectsResponse (Maybe Int)
lorMaxKeys = lens _lorMaxKeys (\ s a -> s{_lorMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria.
lorIsTruncated :: Lens' ListObjectsResponse (Maybe Bool)
lorIsTruncated = lens _lorIsTruncated (\ s a -> s{_lorIsTruncated = a});

-- | FIXME: Undocumented member.
lorDelimiter :: Lens' ListObjectsResponse (Maybe Char)
lorDelimiter = lens _lorDelimiter (\ s a -> s{_lorDelimiter = a});

-- | FIXME: Undocumented member.
lorStatus :: Lens' ListObjectsResponse Int
lorStatus = lens _lorStatus (\ s a -> s{_lorStatus = a});
