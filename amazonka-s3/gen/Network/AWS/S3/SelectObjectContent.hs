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
-- Module      : Network.AWS.S3.SelectObjectContent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation filters the contents of an Amazon S3 object based on a simple Structured Query Language (SQL) statement. In the request, along with the SQL expression, you must also specify a data serialization format (JSON or CSV) of the object. Amazon S3 uses this to parse object data into records, and returns only records that match the specified SQL expression. You must also specify the data serialization format for the response.
module Network.AWS.S3.SelectObjectContent
    (
    -- * Creating a Request
      selectObjectContent
    , SelectObjectContent
    -- * Request Lenses
    , socSSECustomerAlgorithm
    , socSSECustomerKey
    , socRequestProgress
    , socSSECustomerKeyMD5
    , socBucket
    , socKey
    , socExpression
    , socExpressionType
    , socInputSerialization
    , socOutputSerialization

    -- * Destructuring the Response
    , selectObjectContentResponse
    , SelectObjectContentResponse
    -- * Response Lenses
    , socrsPayload
    , socrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectSELECTContent.html S3Select API Documentation>
--
-- /See:/ 'selectObjectContent' smart constructor.
data SelectObjectContent = SelectObjectContent'
  { _socSSECustomerAlgorithm :: !(Maybe Text)
  , _socSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _socRequestProgress      :: !(Maybe RequestProgress)
  , _socSSECustomerKeyMD5    :: !(Maybe Text)
  , _socBucket               :: !BucketName
  , _socKey                  :: !ObjectKey
  , _socExpression           :: !Text
  , _socExpressionType       :: !ExpressionType
  , _socInputSerialization   :: !InputSerialization
  , _socOutputSerialization  :: !OutputSerialization
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectObjectContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socSSECustomerAlgorithm' - <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
--
-- * 'socSSECustomerKey' - <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
--
-- * 'socRequestProgress' - Specifies if periodic request progress information should be enabled.
--
-- * 'socSSECustomerKeyMD5' - <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
--
-- * 'socBucket' - The S3 Bucket.
--
-- * 'socKey' - The Object Key.
--
-- * 'socExpression' - The expression that is used to query the object.
--
-- * 'socExpressionType' - The type of the provided expression (e.g., SQL).
--
-- * 'socInputSerialization' - Describes the format of the data in the object that is being queried.
--
-- * 'socOutputSerialization' - Describes the format of the data that you want Amazon S3 to return in response.
selectObjectContent
    :: BucketName -- ^ 'socBucket'
    -> ObjectKey -- ^ 'socKey'
    -> Text -- ^ 'socExpression'
    -> ExpressionType -- ^ 'socExpressionType'
    -> InputSerialization -- ^ 'socInputSerialization'
    -> OutputSerialization -- ^ 'socOutputSerialization'
    -> SelectObjectContent
selectObjectContent pBucket_ pKey_ pExpression_ pExpressionType_ pInputSerialization_ pOutputSerialization_ =
  SelectObjectContent'
    { _socSSECustomerAlgorithm = Nothing
    , _socSSECustomerKey = Nothing
    , _socRequestProgress = Nothing
    , _socSSECustomerKeyMD5 = Nothing
    , _socBucket = pBucket_
    , _socKey = pKey_
    , _socExpression = pExpression_
    , _socExpressionType = pExpressionType_
    , _socInputSerialization = pInputSerialization_
    , _socOutputSerialization = pOutputSerialization_
    }


-- | <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
socSSECustomerAlgorithm :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerAlgorithm = lens _socSSECustomerAlgorithm (\ s a -> s{_socSSECustomerAlgorithm = a})

-- | <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
socSSECustomerKey :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerKey = lens _socSSECustomerKey (\ s a -> s{_socSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies if periodic request progress information should be enabled.
socRequestProgress :: Lens' SelectObjectContent (Maybe RequestProgress)
socRequestProgress = lens _socRequestProgress (\ s a -> s{_socRequestProgress = a})

-- | <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys>
socSSECustomerKeyMD5 :: Lens' SelectObjectContent (Maybe Text)
socSSECustomerKeyMD5 = lens _socSSECustomerKeyMD5 (\ s a -> s{_socSSECustomerKeyMD5 = a})

-- | The S3 Bucket.
socBucket :: Lens' SelectObjectContent BucketName
socBucket = lens _socBucket (\ s a -> s{_socBucket = a})

-- | The Object Key.
socKey :: Lens' SelectObjectContent ObjectKey
socKey = lens _socKey (\ s a -> s{_socKey = a})

-- | The expression that is used to query the object.
socExpression :: Lens' SelectObjectContent Text
socExpression = lens _socExpression (\ s a -> s{_socExpression = a})

-- | The type of the provided expression (e.g., SQL).
socExpressionType :: Lens' SelectObjectContent ExpressionType
socExpressionType = lens _socExpressionType (\ s a -> s{_socExpressionType = a})

-- | Describes the format of the data in the object that is being queried.
socInputSerialization :: Lens' SelectObjectContent InputSerialization
socInputSerialization = lens _socInputSerialization (\ s a -> s{_socInputSerialization = a})

-- | Describes the format of the data that you want Amazon S3 to return in response.
socOutputSerialization :: Lens' SelectObjectContent OutputSerialization
socOutputSerialization = lens _socOutputSerialization (\ s a -> s{_socOutputSerialization = a})

instance AWSRequest SelectObjectContent where
        type Rs SelectObjectContent =
             SelectObjectContentResponse
        request = postXML s3
        response
          = receiveXML
              (\ s h x ->
                 SelectObjectContentResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable SelectObjectContent where

instance NFData SelectObjectContent where

instance ToElement SelectObjectContent where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}SelectObjectContentRequest"

instance ToHeaders SelectObjectContent where
        toHeaders SelectObjectContent'{..}
          = mconcat
              ["x-amz-server-side-encryption-customer-algorithm" =#
                 _socSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _socSSECustomerKey,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _socSSECustomerKeyMD5]

instance ToPath SelectObjectContent where
        toPath SelectObjectContent'{..}
          = mconcat ["/", toBS _socBucket, "/", toBS _socKey]

instance ToQuery SelectObjectContent where
        toQuery = const (mconcat ["select&select-type=2"])

instance ToXML SelectObjectContent where
        toXML SelectObjectContent'{..}
          = mconcat
              ["RequestProgress" @= _socRequestProgress,
               "Expression" @= _socExpression,
               "ExpressionType" @= _socExpressionType,
               "InputSerialization" @= _socInputSerialization,
               "OutputSerialization" @= _socOutputSerialization]

-- | /See:/ 'selectObjectContentResponse' smart constructor.
data SelectObjectContentResponse = SelectObjectContentResponse'
  { _socrsPayload        :: !(Maybe SelectObjectContentEventStream)
  , _socrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectObjectContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socrsPayload' - Undocumented member.
--
-- * 'socrsResponseStatus' - -- | The response status code.
selectObjectContentResponse
    :: Int -- ^ 'socrsResponseStatus'
    -> SelectObjectContentResponse
selectObjectContentResponse pResponseStatus_ =
  SelectObjectContentResponse'
    {_socrsPayload = Nothing, _socrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
socrsPayload :: Lens' SelectObjectContentResponse (Maybe SelectObjectContentEventStream)
socrsPayload = lens _socrsPayload (\ s a -> s{_socrsPayload = a})

-- | -- | The response status code.
socrsResponseStatus :: Lens' SelectObjectContentResponse Int
socrsResponseStatus = lens _socrsResponseStatus (\ s a -> s{_socrsResponseStatus = a})

instance NFData SelectObjectContentResponse where
