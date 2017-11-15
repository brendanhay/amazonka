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
-- Module      : Network.AWS.S3.ListObjectsV
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.
module Network.AWS.S3.ListObjectsV
    (
    -- * Creating a Request
      listObjectsV
    , ListObjectsV
    -- * Request Lenses
    , lStartAfter
    , lContinuationToken
    , lFetchOwner
    , lPrefix
    , lEncodingType
    , lRequestPayer
    , lMaxKeys
    , lDelimiter
    , lBucket

    -- * Destructuring the Response
    , listObjectsVResponse
    , ListObjectsVResponse
    -- * Response Lenses
    , lrsStartAfter
    , lrsKeyCount
    , lrsContents
    , lrsContinuationToken
    , lrsPrefix
    , lrsCommonPrefixes
    , lrsEncodingType
    , lrsName
    , lrsNextContinuationToken
    , lrsMaxKeys
    , lrsIsTruncated
    , lrsDelimiter
    , lrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listObjectsV' smart constructor.
data ListObjectsV = ListObjectsV'
  { _lStartAfter        :: !(Maybe Text)
  , _lContinuationToken :: !(Maybe Text)
  , _lFetchOwner        :: !(Maybe Bool)
  , _lPrefix            :: !(Maybe Text)
  , _lEncodingType      :: !(Maybe EncodingType)
  , _lRequestPayer      :: !(Maybe RequestPayer)
  , _lMaxKeys           :: !(Maybe Int)
  , _lDelimiter         :: !(Maybe Delimiter)
  , _lBucket            :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectsV' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lStartAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
--
-- * 'lContinuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
--
-- * 'lFetchOwner' - The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true
--
-- * 'lPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lRequestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
--
-- * 'lMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lBucket' - Name of the bucket to list.
listObjectsV
    :: BucketName -- ^ 'lBucket'
    -> ListObjectsV
listObjectsV pBucket_ =
  ListObjectsV'
  { _lStartAfter = Nothing
  , _lContinuationToken = Nothing
  , _lFetchOwner = Nothing
  , _lPrefix = Nothing
  , _lEncodingType = Nothing
  , _lRequestPayer = Nothing
  , _lMaxKeys = Nothing
  , _lDelimiter = Nothing
  , _lBucket = pBucket_
  }


-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
lStartAfter :: Lens' ListObjectsV (Maybe Text)
lStartAfter = lens _lStartAfter (\ s a -> s{_lStartAfter = a});

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
lContinuationToken :: Lens' ListObjectsV (Maybe Text)
lContinuationToken = lens _lContinuationToken (\ s a -> s{_lContinuationToken = a});

-- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true
lFetchOwner :: Lens' ListObjectsV (Maybe Bool)
lFetchOwner = lens _lFetchOwner (\ s a -> s{_lFetchOwner = a});

-- | Limits the response to keys that begin with the specified prefix.
lPrefix :: Lens' ListObjectsV (Maybe Text)
lPrefix = lens _lPrefix (\ s a -> s{_lPrefix = a});

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lEncodingType :: Lens' ListObjectsV (Maybe EncodingType)
lEncodingType = lens _lEncodingType (\ s a -> s{_lEncodingType = a});

-- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
lRequestPayer :: Lens' ListObjectsV (Maybe RequestPayer)
lRequestPayer = lens _lRequestPayer (\ s a -> s{_lRequestPayer = a});

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lMaxKeys :: Lens' ListObjectsV (Maybe Int)
lMaxKeys = lens _lMaxKeys (\ s a -> s{_lMaxKeys = a});

-- | A delimiter is a character you use to group keys.
lDelimiter :: Lens' ListObjectsV (Maybe Delimiter)
lDelimiter = lens _lDelimiter (\ s a -> s{_lDelimiter = a});

-- | Name of the bucket to list.
lBucket :: Lens' ListObjectsV BucketName
lBucket = lens _lBucket (\ s a -> s{_lBucket = a});

instance AWSRequest ListObjectsV where
        type Rs ListObjectsV = ListObjectsVResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListObjectsVResponse' <$>
                   (x .@? "StartAfter") <*> (x .@? "KeyCount") <*>
                     (may (parseXMLList "Contents") x)
                     <*> (x .@? "ContinuationToken")
                     <*> (x .@? "Prefix")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "EncodingType")
                     <*> (x .@? "Name")
                     <*> (x .@? "NextContinuationToken")
                     <*> (x .@? "MaxKeys")
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectsV where

instance NFData ListObjectsV where

instance ToHeaders ListObjectsV where
        toHeaders ListObjectsV'{..}
          = mconcat ["x-amz-request-payer" =# _lRequestPayer]

instance ToPath ListObjectsV where
        toPath ListObjectsV'{..}
          = mconcat ["/", toBS _lBucket]

instance ToQuery ListObjectsV where
        toQuery ListObjectsV'{..}
          = mconcat
              ["start-after" =: _lStartAfter,
               "continuation-token" =: _lContinuationToken,
               "fetch-owner" =: _lFetchOwner, "prefix" =: _lPrefix,
               "encoding-type" =: _lEncodingType,
               "max-keys" =: _lMaxKeys, "delimiter" =: _lDelimiter,
               "list-type=2"]

-- | /See:/ 'listObjectsVResponse' smart constructor.
data ListObjectsVResponse = ListObjectsVResponse'
  { _lrsStartAfter            :: !(Maybe Text)
  , _lrsKeyCount              :: !(Maybe Int)
  , _lrsContents              :: !(Maybe [Object])
  , _lrsContinuationToken     :: !(Maybe Text)
  , _lrsPrefix                :: !(Maybe Text)
  , _lrsCommonPrefixes        :: !(Maybe [CommonPrefix])
  , _lrsEncodingType          :: !(Maybe EncodingType)
  , _lrsName                  :: !(Maybe BucketName)
  , _lrsNextContinuationToken :: !(Maybe Text)
  , _lrsMaxKeys               :: !(Maybe Int)
  , _lrsIsTruncated           :: !(Maybe Bool)
  , _lrsDelimiter             :: !(Maybe Delimiter)
  , _lrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectsVResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsStartAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
--
-- * 'lrsKeyCount' - KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
--
-- * 'lrsContents' - Metadata about each object returned.
--
-- * 'lrsContinuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
--
-- * 'lrsPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lrsCommonPrefixes' - CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
--
-- * 'lrsEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lrsName' - Name of the bucket to list.
--
-- * 'lrsNextContinuationToken' - NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
--
-- * 'lrsMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lrsIsTruncated' - A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
--
-- * 'lrsDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listObjectsVResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListObjectsVResponse
listObjectsVResponse pResponseStatus_ =
  ListObjectsVResponse'
  { _lrsStartAfter = Nothing
  , _lrsKeyCount = Nothing
  , _lrsContents = Nothing
  , _lrsContinuationToken = Nothing
  , _lrsPrefix = Nothing
  , _lrsCommonPrefixes = Nothing
  , _lrsEncodingType = Nothing
  , _lrsName = Nothing
  , _lrsNextContinuationToken = Nothing
  , _lrsMaxKeys = Nothing
  , _lrsIsTruncated = Nothing
  , _lrsDelimiter = Nothing
  , _lrsResponseStatus = pResponseStatus_
  }


-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
lrsStartAfter :: Lens' ListObjectsVResponse (Maybe Text)
lrsStartAfter = lens _lrsStartAfter (\ s a -> s{_lrsStartAfter = a});

-- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
lrsKeyCount :: Lens' ListObjectsVResponse (Maybe Int)
lrsKeyCount = lens _lrsKeyCount (\ s a -> s{_lrsKeyCount = a});

-- | Metadata about each object returned.
lrsContents :: Lens' ListObjectsVResponse [Object]
lrsContents = lens _lrsContents (\ s a -> s{_lrsContents = a}) . _Default . _Coerce;

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
lrsContinuationToken :: Lens' ListObjectsVResponse (Maybe Text)
lrsContinuationToken = lens _lrsContinuationToken (\ s a -> s{_lrsContinuationToken = a});

-- | Limits the response to keys that begin with the specified prefix.
lrsPrefix :: Lens' ListObjectsVResponse (Maybe Text)
lrsPrefix = lens _lrsPrefix (\ s a -> s{_lrsPrefix = a});

-- | CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
lrsCommonPrefixes :: Lens' ListObjectsVResponse [CommonPrefix]
lrsCommonPrefixes = lens _lrsCommonPrefixes (\ s a -> s{_lrsCommonPrefixes = a}) . _Default . _Coerce;

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lrsEncodingType :: Lens' ListObjectsVResponse (Maybe EncodingType)
lrsEncodingType = lens _lrsEncodingType (\ s a -> s{_lrsEncodingType = a});

-- | Name of the bucket to list.
lrsName :: Lens' ListObjectsVResponse (Maybe BucketName)
lrsName = lens _lrsName (\ s a -> s{_lrsName = a});

-- | NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
lrsNextContinuationToken :: Lens' ListObjectsVResponse (Maybe Text)
lrsNextContinuationToken = lens _lrsNextContinuationToken (\ s a -> s{_lrsNextContinuationToken = a});

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lrsMaxKeys :: Lens' ListObjectsVResponse (Maybe Int)
lrsMaxKeys = lens _lrsMaxKeys (\ s a -> s{_lrsMaxKeys = a});

-- | A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
lrsIsTruncated :: Lens' ListObjectsVResponse (Maybe Bool)
lrsIsTruncated = lens _lrsIsTruncated (\ s a -> s{_lrsIsTruncated = a});

-- | A delimiter is a character you use to group keys.
lrsDelimiter :: Lens' ListObjectsVResponse (Maybe Delimiter)
lrsDelimiter = lens _lrsDelimiter (\ s a -> s{_lrsDelimiter = a});

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListObjectsVResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a});

instance NFData ListObjectsVResponse where
