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
    , lorqPrefix
    , lorqEncodingType
    , lorqMarker
    , lorqMaxKeys
    , lorqDelimiter
    , lorqBucket

    -- * Response
    , ListObjectsResponse
    -- ** Response constructor
    , listObjectsResponse
    -- ** Response lenses
    , lorsContents
    , lorsPrefix
    , lorsEncodingType
    , lorsCommonPrefixes
    , lorsName
    , lorsMarker
    , lorsNextMarker
    , lorsMaxKeys
    , lorsIsTruncated
    , lorsDelimiter
    , lorsStatus
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
-- * 'lorqPrefix'
--
-- * 'lorqEncodingType'
--
-- * 'lorqMarker'
--
-- * 'lorqMaxKeys'
--
-- * 'lorqDelimiter'
--
-- * 'lorqBucket'
data ListObjects = ListObjects'
    { _lorqPrefix       :: !(Maybe Text)
    , _lorqEncodingType :: !(Maybe EncodingType)
    , _lorqMarker       :: !(Maybe Text)
    , _lorqMaxKeys      :: !(Maybe Int)
    , _lorqDelimiter    :: !(Maybe Char)
    , _lorqBucket       :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjects' smart constructor.
listObjects :: BucketName -> ListObjects
listObjects pBucket_ =
    ListObjects'
    { _lorqPrefix = Nothing
    , _lorqEncodingType = Nothing
    , _lorqMarker = Nothing
    , _lorqMaxKeys = Nothing
    , _lorqDelimiter = Nothing
    , _lorqBucket = pBucket_
    }

-- | Limits the response to keys that begin with the specified prefix.
lorqPrefix :: Lens' ListObjects (Maybe Text)
lorqPrefix = lens _lorqPrefix (\ s a -> s{_lorqPrefix = a});

-- | FIXME: Undocumented member.
lorqEncodingType :: Lens' ListObjects (Maybe EncodingType)
lorqEncodingType = lens _lorqEncodingType (\ s a -> s{_lorqEncodingType = a});

-- | Specifies the key to start with when listing objects in a bucket.
lorqMarker :: Lens' ListObjects (Maybe Text)
lorqMarker = lens _lorqMarker (\ s a -> s{_lorqMarker = a});

-- | Sets the maximum number of keys returned in the response. The response
-- might contain fewer keys but will never contain more.
lorqMaxKeys :: Lens' ListObjects (Maybe Int)
lorqMaxKeys = lens _lorqMaxKeys (\ s a -> s{_lorqMaxKeys = a});

-- | A delimiter is a character you use to group keys.
lorqDelimiter :: Lens' ListObjects (Maybe Char)
lorqDelimiter = lens _lorqDelimiter (\ s a -> s{_lorqDelimiter = a});

-- | FIXME: Undocumented member.
lorqBucket :: Lens' ListObjects BucketName
lorqBucket = lens _lorqBucket (\ s a -> s{_lorqBucket = a});

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
              lorqMarker .~
                rs ^.
                  choice (^. lorsNextMarker)
                    (^? (lorsContents . _last . oKey))

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
          = mconcat ["/", toText _lorqBucket]

instance ToQuery ListObjects where
        toQuery ListObjects'{..}
          = mconcat
              ["prefix" =: _lorqPrefix,
               "encoding-type" =: _lorqEncodingType,
               "marker" =: _lorqMarker, "max-keys" =: _lorqMaxKeys,
               "delimiter" =: _lorqDelimiter]

-- | /See:/ 'listObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorsContents'
--
-- * 'lorsPrefix'
--
-- * 'lorsEncodingType'
--
-- * 'lorsCommonPrefixes'
--
-- * 'lorsName'
--
-- * 'lorsMarker'
--
-- * 'lorsNextMarker'
--
-- * 'lorsMaxKeys'
--
-- * 'lorsIsTruncated'
--
-- * 'lorsDelimiter'
--
-- * 'lorsStatus'
data ListObjectsResponse = ListObjectsResponse'
    { _lorsContents       :: !(Maybe [Object])
    , _lorsPrefix         :: !(Maybe Text)
    , _lorsEncodingType   :: !(Maybe EncodingType)
    , _lorsCommonPrefixes :: !(Maybe [CommonPrefix])
    , _lorsName           :: !(Maybe BucketName)
    , _lorsMarker         :: !(Maybe Text)
    , _lorsNextMarker     :: !(Maybe Text)
    , _lorsMaxKeys        :: !(Maybe Int)
    , _lorsIsTruncated    :: !(Maybe Bool)
    , _lorsDelimiter      :: !(Maybe Char)
    , _lorsStatus         :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ListObjectsResponse' smart constructor.
listObjectsResponse :: Int -> ListObjectsResponse
listObjectsResponse pStatus_ =
    ListObjectsResponse'
    { _lorsContents = Nothing
    , _lorsPrefix = Nothing
    , _lorsEncodingType = Nothing
    , _lorsCommonPrefixes = Nothing
    , _lorsName = Nothing
    , _lorsMarker = Nothing
    , _lorsNextMarker = Nothing
    , _lorsMaxKeys = Nothing
    , _lorsIsTruncated = Nothing
    , _lorsDelimiter = Nothing
    , _lorsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
lorsContents :: Lens' ListObjectsResponse [Object]
lorsContents = lens _lorsContents (\ s a -> s{_lorsContents = a}) . _Default;

-- | FIXME: Undocumented member.
lorsPrefix :: Lens' ListObjectsResponse (Maybe Text)
lorsPrefix = lens _lorsPrefix (\ s a -> s{_lorsPrefix = a});

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lorsEncodingType :: Lens' ListObjectsResponse (Maybe EncodingType)
lorsEncodingType = lens _lorsEncodingType (\ s a -> s{_lorsEncodingType = a});

-- | FIXME: Undocumented member.
lorsCommonPrefixes :: Lens' ListObjectsResponse [CommonPrefix]
lorsCommonPrefixes = lens _lorsCommonPrefixes (\ s a -> s{_lorsCommonPrefixes = a}) . _Default;

-- | FIXME: Undocumented member.
lorsName :: Lens' ListObjectsResponse (Maybe BucketName)
lorsName = lens _lorsName (\ s a -> s{_lorsName = a});

-- | FIXME: Undocumented member.
lorsMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsMarker = lens _lorsMarker (\ s a -> s{_lorsMarker = a});

-- | When response is truncated (the IsTruncated element value in the
-- response is true), you can use the key name in this field as marker in
-- the subsequent request to get next set of objects. Amazon S3 lists
-- objects in alphabetical order Note: This element is returned only if you
-- have delimiter request parameter specified. If response does not include
-- the NextMaker and it is truncated, you can use the value of the last Key
-- in the response as the marker in the subsequent request to get the next
-- set of object keys.
lorsNextMarker :: Lens' ListObjectsResponse (Maybe Text)
lorsNextMarker = lens _lorsNextMarker (\ s a -> s{_lorsNextMarker = a});

-- | FIXME: Undocumented member.
lorsMaxKeys :: Lens' ListObjectsResponse (Maybe Int)
lorsMaxKeys = lens _lorsMaxKeys (\ s a -> s{_lorsMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the
-- results that satisfied the search criteria.
lorsIsTruncated :: Lens' ListObjectsResponse (Maybe Bool)
lorsIsTruncated = lens _lorsIsTruncated (\ s a -> s{_lorsIsTruncated = a});

-- | FIXME: Undocumented member.
lorsDelimiter :: Lens' ListObjectsResponse (Maybe Char)
lorsDelimiter = lens _lorsDelimiter (\ s a -> s{_lorsDelimiter = a});

-- | FIXME: Undocumented member.
lorsStatus :: Lens' ListObjectsResponse Int
lorsStatus = lens _lorsStatus (\ s a -> s{_lorsStatus = a});
