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
-- Module      : Network.AWS.S3.ListObjectsV2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectsV2
    (
    -- * Creating a Request
      listObjectsV2
    , ListObjectsV2
    -- * Request Lenses
    , lovStartAfter
    , lovContinuationToken
    , lovFetchOwner
    , lovPrefix
    , lovEncodingType
    , lovRequestPayer
    , lovMaxKeys
    , lovDelimiter
    , lovBucket

    -- * Destructuring the Response
    , listObjectsV2Response
    , ListObjectsV2Response
    -- * Response Lenses
    , lovrsStartAfter
    , lovrsKeyCount
    , lovrsContents
    , lovrsContinuationToken
    , lovrsPrefix
    , lovrsCommonPrefixes
    , lovrsEncodingType
    , lovrsName
    , lovrsNextContinuationToken
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

-- | /See:/ 'listObjectsV2' smart constructor.
data ListObjectsV2 = ListObjectsV2'
  { _lovStartAfter        :: !(Maybe Text)
  , _lovContinuationToken :: !(Maybe Text)
  , _lovFetchOwner        :: !(Maybe Bool)
  , _lovPrefix            :: !(Maybe Text)
  , _lovEncodingType      :: !(Maybe EncodingType)
  , _lovRequestPayer      :: !(Maybe RequestPayer)
  , _lovMaxKeys           :: !(Maybe Int)
  , _lovDelimiter         :: !(Maybe Delimiter)
  , _lovBucket            :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectsV2' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovStartAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
--
-- * 'lovContinuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
--
-- * 'lovFetchOwner' - The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true
--
-- * 'lovPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lovEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lovRequestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
--
-- * 'lovMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lovDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lovBucket' - Name of the bucket to list.
listObjectsV2
    :: BucketName -- ^ 'lovBucket'
    -> ListObjectsV2
listObjectsV2 pBucket_ =
  ListObjectsV2'
    { _lovStartAfter = Nothing
    , _lovContinuationToken = Nothing
    , _lovFetchOwner = Nothing
    , _lovPrefix = Nothing
    , _lovEncodingType = Nothing
    , _lovRequestPayer = Nothing
    , _lovMaxKeys = Nothing
    , _lovDelimiter = Nothing
    , _lovBucket = pBucket_
    }


-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
lovStartAfter :: Lens' ListObjectsV2 (Maybe Text)
lovStartAfter = lens _lovStartAfter (\ s a -> s{_lovStartAfter = a})

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
lovContinuationToken :: Lens' ListObjectsV2 (Maybe Text)
lovContinuationToken = lens _lovContinuationToken (\ s a -> s{_lovContinuationToken = a})

-- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true
lovFetchOwner :: Lens' ListObjectsV2 (Maybe Bool)
lovFetchOwner = lens _lovFetchOwner (\ s a -> s{_lovFetchOwner = a})

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectsV2 (Maybe Text)
lovPrefix = lens _lovPrefix (\ s a -> s{_lovPrefix = a})

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovEncodingType :: Lens' ListObjectsV2 (Maybe EncodingType)
lovEncodingType = lens _lovEncodingType (\ s a -> s{_lovEncodingType = a})

-- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
lovRequestPayer :: Lens' ListObjectsV2 (Maybe RequestPayer)
lovRequestPayer = lens _lovRequestPayer (\ s a -> s{_lovRequestPayer = a})

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectsV2 (Maybe Int)
lovMaxKeys = lens _lovMaxKeys (\ s a -> s{_lovMaxKeys = a})

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectsV2 (Maybe Delimiter)
lovDelimiter = lens _lovDelimiter (\ s a -> s{_lovDelimiter = a})

-- | Name of the bucket to list.
lovBucket :: Lens' ListObjectsV2 BucketName
lovBucket = lens _lovBucket (\ s a -> s{_lovBucket = a})

instance AWSPager ListObjectsV2 where
        page rq rs
          | stop (rs ^. lovrsIsTruncated) = Nothing
          | isNothing (rs ^. lovrsNextContinuationToken) =
            Nothing
          | otherwise =
            Just $ rq &
              lovContinuationToken .~
                rs ^. lovrsNextContinuationToken

instance AWSRequest ListObjectsV2 where
        type Rs ListObjectsV2 = ListObjectsV2Response
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListObjectsV2Response' <$>
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

instance Hashable ListObjectsV2 where

instance NFData ListObjectsV2 where

instance ToHeaders ListObjectsV2 where
        toHeaders ListObjectsV2'{..}
          = mconcat ["x-amz-request-payer" =# _lovRequestPayer]

instance ToPath ListObjectsV2 where
        toPath ListObjectsV2'{..}
          = mconcat ["/", toBS _lovBucket]

instance ToQuery ListObjectsV2 where
        toQuery ListObjectsV2'{..}
          = mconcat
              ["start-after" =: _lovStartAfter,
               "continuation-token" =: _lovContinuationToken,
               "fetch-owner" =: _lovFetchOwner,
               "prefix" =: _lovPrefix,
               "encoding-type" =: _lovEncodingType,
               "max-keys" =: _lovMaxKeys,
               "delimiter" =: _lovDelimiter, "list-type=2"]

-- | /See:/ 'listObjectsV2Response' smart constructor.
data ListObjectsV2Response = ListObjectsV2Response'
  { _lovrsStartAfter            :: !(Maybe Text)
  , _lovrsKeyCount              :: !(Maybe Int)
  , _lovrsContents              :: !(Maybe [Object])
  , _lovrsContinuationToken     :: !(Maybe Text)
  , _lovrsPrefix                :: !(Maybe Text)
  , _lovrsCommonPrefixes        :: !(Maybe [CommonPrefix])
  , _lovrsEncodingType          :: !(Maybe EncodingType)
  , _lovrsName                  :: !(Maybe BucketName)
  , _lovrsNextContinuationToken :: !(Maybe Text)
  , _lovrsMaxKeys               :: !(Maybe Int)
  , _lovrsIsTruncated           :: !(Maybe Bool)
  , _lovrsDelimiter             :: !(Maybe Delimiter)
  , _lovrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectsV2Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovrsStartAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
--
-- * 'lovrsKeyCount' - KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
--
-- * 'lovrsContents' - Metadata about each object returned.
--
-- * 'lovrsContinuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
--
-- * 'lovrsPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lovrsCommonPrefixes' - CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
--
-- * 'lovrsEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lovrsName' - Name of the bucket to list.
--
-- * 'lovrsNextContinuationToken' - NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
--
-- * 'lovrsMaxKeys' - Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
--
-- * 'lovrsIsTruncated' - A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
--
-- * 'lovrsDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lovrsResponseStatus' - -- | The response status code.
listObjectsV2Response
    :: Int -- ^ 'lovrsResponseStatus'
    -> ListObjectsV2Response
listObjectsV2Response pResponseStatus_ =
  ListObjectsV2Response'
    { _lovrsStartAfter = Nothing
    , _lovrsKeyCount = Nothing
    , _lovrsContents = Nothing
    , _lovrsContinuationToken = Nothing
    , _lovrsPrefix = Nothing
    , _lovrsCommonPrefixes = Nothing
    , _lovrsEncodingType = Nothing
    , _lovrsName = Nothing
    , _lovrsNextContinuationToken = Nothing
    , _lovrsMaxKeys = Nothing
    , _lovrsIsTruncated = Nothing
    , _lovrsDelimiter = Nothing
    , _lovrsResponseStatus = pResponseStatus_
    }


-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket
lovrsStartAfter :: Lens' ListObjectsV2Response (Maybe Text)
lovrsStartAfter = lens _lovrsStartAfter (\ s a -> s{_lovrsStartAfter = a})

-- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
lovrsKeyCount :: Lens' ListObjectsV2Response (Maybe Int)
lovrsKeyCount = lens _lovrsKeyCount (\ s a -> s{_lovrsKeyCount = a})

-- | Metadata about each object returned.
lovrsContents :: Lens' ListObjectsV2Response [Object]
lovrsContents = lens _lovrsContents (\ s a -> s{_lovrsContents = a}) . _Default . _Coerce

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key
lovrsContinuationToken :: Lens' ListObjectsV2Response (Maybe Text)
lovrsContinuationToken = lens _lovrsContinuationToken (\ s a -> s{_lovrsContinuationToken = a})

-- | Limits the response to keys that begin with the specified prefix.
lovrsPrefix :: Lens' ListObjectsV2Response (Maybe Text)
lovrsPrefix = lens _lovrsPrefix (\ s a -> s{_lovrsPrefix = a})

-- | CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by delimiter
lovrsCommonPrefixes :: Lens' ListObjectsV2Response [CommonPrefix]
lovrsCommonPrefixes = lens _lovrsCommonPrefixes (\ s a -> s{_lovrsCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovrsEncodingType :: Lens' ListObjectsV2Response (Maybe EncodingType)
lovrsEncodingType = lens _lovrsEncodingType (\ s a -> s{_lovrsEncodingType = a})

-- | Name of the bucket to list.
lovrsName :: Lens' ListObjectsV2Response (Maybe BucketName)
lovrsName = lens _lovrsName (\ s a -> s{_lovrsName = a})

-- | NextContinuationToken is sent when isTruncated is true which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this NextContinuationToken. NextContinuationToken is obfuscated and is not a real key
lovrsNextContinuationToken :: Lens' ListObjectsV2Response (Maybe Text)
lovrsNextContinuationToken = lens _lovrsNextContinuationToken (\ s a -> s{_lovrsNextContinuationToken = a})

-- | Sets the maximum number of keys returned in the response. The response might contain fewer keys but will never contain more.
lovrsMaxKeys :: Lens' ListObjectsV2Response (Maybe Int)
lovrsMaxKeys = lens _lovrsMaxKeys (\ s a -> s{_lovrsMaxKeys = a})

-- | A flag that indicates whether or not Amazon S3 returned all of the results that satisfied the search criteria.
lovrsIsTruncated :: Lens' ListObjectsV2Response (Maybe Bool)
lovrsIsTruncated = lens _lovrsIsTruncated (\ s a -> s{_lovrsIsTruncated = a})

-- | A delimiter is a character you use to group keys.
lovrsDelimiter :: Lens' ListObjectsV2Response (Maybe Delimiter)
lovrsDelimiter = lens _lovrsDelimiter (\ s a -> s{_lovrsDelimiter = a})

-- | -- | The response status code.
lovrsResponseStatus :: Lens' ListObjectsV2Response Int
lovrsResponseStatus = lens _lovrsResponseStatus (\ s a -> s{_lovrsResponseStatus = a})

instance NFData ListObjectsV2Response where
