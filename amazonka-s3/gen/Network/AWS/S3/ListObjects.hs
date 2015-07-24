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
listObjects pBucket_ =
    ListObjects'
    { _loPrefix = Nothing
    , _loEncodingType = Nothing
    , _loMarker = Nothing
    , _loMaxKeys = Nothing
    , _loDelimiter = Nothing
    , _loBucket = pBucket_
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
        type Sv ListObjects = S3
        type Rs ListObjects = ListObjectsResponse
        request = get "ListObjects"
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
