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
-- Updates the code for the specified Lambda function. This operation must only be used on an existing Lambda function and cannot be used to update the function configuration.
--
--
-- If you are using the versioning feature, note this API will always update the $LATEST version of your Lambda function. For information about the versioning feature, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:UpdateFunctionCode@ action.
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

-- |
--
--
--
-- /See:/ 'updateFunctionCode' smart constructor.
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
-- * 'uS3ObjectVersion' - The Amazon S3 object (the deployment package) version you want to upload.
--
-- * 'uS3Key' - The Amazon S3 object (the deployment package) key name you want to upload.
--
-- * 'uZipFile' - The contents of your zip file containing your deployment package. If you are using the web API directly, the contents of the zip file must be base64-encoded. If you are using the AWS SDKs or the AWS CLI, the SDKs or CLI will do the encoding for you. For more information about creating a .zip file, see <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions> . -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'uS3Bucket' - Amazon S3 bucket name where the .zip file containing your deployment package is stored. This bucket must reside in the same AWS Region where you are creating the Lambda function.
--
-- * 'uDryRun' - This boolean parameter can be used to test your request to AWS Lambda to update the Lambda function and publish a version as an atomic operation. It will do all necessary computation and validation of your code but will not upload it or a publish a version. Each time this operation is invoked, the @CodeSha256@ hash value of the provided code will also be computed and returned in the response.
--
-- * 'uRevisionId' - An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
--
-- * 'uPublish' - This boolean parameter can be used to request AWS Lambda to update the Lambda function and publish a version as an atomic operation.
--
-- * 'uFunctionName' - The existing Lambda function name whose code you want to replace. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
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


-- | The Amazon S3 object (the deployment package) version you want to upload.
uS3ObjectVersion :: Lens' UpdateFunctionCode (Maybe Text)
uS3ObjectVersion = lens _uS3ObjectVersion (\ s a -> s{_uS3ObjectVersion = a})

-- | The Amazon S3 object (the deployment package) key name you want to upload.
uS3Key :: Lens' UpdateFunctionCode (Maybe Text)
uS3Key = lens _uS3Key (\ s a -> s{_uS3Key = a})

-- | The contents of your zip file containing your deployment package. If you are using the web API directly, the contents of the zip file must be base64-encoded. If you are using the AWS SDKs or the AWS CLI, the SDKs or CLI will do the encoding for you. For more information about creating a .zip file, see <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions> . -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
uZipFile :: Lens' UpdateFunctionCode (Maybe ByteString)
uZipFile = lens _uZipFile (\ s a -> s{_uZipFile = a}) . mapping (_Sensitive . _Base64)

-- | Amazon S3 bucket name where the .zip file containing your deployment package is stored. This bucket must reside in the same AWS Region where you are creating the Lambda function.
uS3Bucket :: Lens' UpdateFunctionCode (Maybe Text)
uS3Bucket = lens _uS3Bucket (\ s a -> s{_uS3Bucket = a})

-- | This boolean parameter can be used to test your request to AWS Lambda to update the Lambda function and publish a version as an atomic operation. It will do all necessary computation and validation of your code but will not upload it or a publish a version. Each time this operation is invoked, the @CodeSha256@ hash value of the provided code will also be computed and returned in the response.
uDryRun :: Lens' UpdateFunctionCode (Maybe Bool)
uDryRun = lens _uDryRun (\ s a -> s{_uDryRun = a})

-- | An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
uRevisionId :: Lens' UpdateFunctionCode (Maybe Text)
uRevisionId = lens _uRevisionId (\ s a -> s{_uRevisionId = a})

-- | This boolean parameter can be used to request AWS Lambda to update the Lambda function and publish a version as an atomic operation.
uPublish :: Lens' UpdateFunctionCode (Maybe Bool)
uPublish = lens _uPublish (\ s a -> s{_uPublish = a})

-- | The existing Lambda function name whose code you want to replace. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
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
