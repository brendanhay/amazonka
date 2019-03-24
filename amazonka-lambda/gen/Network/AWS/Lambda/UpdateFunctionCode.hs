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
-- Module      : Network.AWS.Lambda.UpdateFunctionCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function's code.
--
--
-- The function's code is locked when you publish a version. You can't modify the code of a published version, only the unpublished version.
--
module Network.AWS.Lambda.UpdateFunctionCode
    (
    -- * Creating a Request
      updateFunctionCode
    , UpdateFunctionCode
    -- * Request Lenses
    , uS3ObjectVersion
    , uS3Key
    , uZipFile
    , uS3Bucket
    , uDryRun
    , uRevisionId
    , uPublish
    , uFunctionName

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcMemorySize
    , fcRuntime
    , fcFunctionARN
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcLayers
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcDescription
    , fcRevisionId
    , fcMasterARN
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
  { _uS3ObjectVersion :: !(Maybe Text)
  , _uS3Key           :: !(Maybe Text)
  , _uZipFile         :: !(Maybe (Sensitive Base64))
  , _uS3Bucket        :: !(Maybe Text)
  , _uDryRun          :: !(Maybe Bool)
  , _uRevisionId      :: !(Maybe Text)
  , _uPublish         :: !(Maybe Bool)
  , _uFunctionName    :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFunctionCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uS3ObjectVersion' - For versioned objects, the version of the deployment package object to use.
--
-- * 'uS3Key' - The Amazon S3 key of the deployment package.
--
-- * 'uZipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'uS3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
--
-- * 'uDryRun' - Set to true to validate the request parameters and access permissions without modifying the function code.
--
-- * 'uRevisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
--
-- * 'uPublish' - Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
--
-- * 'uFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
updateFunctionCode
    :: Text -- ^ 'uFunctionName'
    -> UpdateFunctionCode
updateFunctionCode pFunctionName_ =
  UpdateFunctionCode'
    { _uS3ObjectVersion = Nothing
    , _uS3Key = Nothing
    , _uZipFile = Nothing
    , _uS3Bucket = Nothing
    , _uDryRun = Nothing
    , _uRevisionId = Nothing
    , _uPublish = Nothing
    , _uFunctionName = pFunctionName_
    }


-- | For versioned objects, the version of the deployment package object to use.
uS3ObjectVersion :: Lens' UpdateFunctionCode (Maybe Text)
uS3ObjectVersion = lens _uS3ObjectVersion (\ s a -> s{_uS3ObjectVersion = a})

-- | The Amazon S3 key of the deployment package.
uS3Key :: Lens' UpdateFunctionCode (Maybe Text)
uS3Key = lens _uS3Key (\ s a -> s{_uS3Key = a})

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
uZipFile :: Lens' UpdateFunctionCode (Maybe ByteString)
uZipFile = lens _uZipFile (\ s a -> s{_uZipFile = a}) . mapping (_Sensitive . _Base64)

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
uS3Bucket :: Lens' UpdateFunctionCode (Maybe Text)
uS3Bucket = lens _uS3Bucket (\ s a -> s{_uS3Bucket = a})

-- | Set to true to validate the request parameters and access permissions without modifying the function code.
uDryRun :: Lens' UpdateFunctionCode (Maybe Bool)
uDryRun = lens _uDryRun (\ s a -> s{_uDryRun = a})

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid modifying a function that has changed since you last read it.
uRevisionId :: Lens' UpdateFunctionCode (Maybe Text)
uRevisionId = lens _uRevisionId (\ s a -> s{_uRevisionId = a})

-- | Set to true to publish a new version of the function after updating the code. This has the same effect as calling 'PublishVersion' separately.
uPublish :: Lens' UpdateFunctionCode (Maybe Bool)
uPublish = lens _uPublish (\ s a -> s{_uPublish = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
uFunctionName :: Lens' UpdateFunctionCode Text
uFunctionName = lens _uFunctionName (\ s a -> s{_uFunctionName = a})

instance AWSRequest UpdateFunctionCode where
        type Rs UpdateFunctionCode = FunctionConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateFunctionCode where

instance NFData UpdateFunctionCode where

instance ToHeaders UpdateFunctionCode where
        toHeaders = const mempty

instance ToJSON UpdateFunctionCode where
        toJSON UpdateFunctionCode'{..}
          = object
              (catMaybes
                 [("S3ObjectVersion" .=) <$> _uS3ObjectVersion,
                  ("S3Key" .=) <$> _uS3Key,
                  ("ZipFile" .=) <$> _uZipFile,
                  ("S3Bucket" .=) <$> _uS3Bucket,
                  ("DryRun" .=) <$> _uDryRun,
                  ("RevisionId" .=) <$> _uRevisionId,
                  ("Publish" .=) <$> _uPublish])

instance ToPath UpdateFunctionCode where
        toPath UpdateFunctionCode'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _uFunctionName,
               "/code"]

instance ToQuery UpdateFunctionCode where
        toQuery = const mempty
