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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the code for the specified Lambda function. This operation must
-- only be used on an existing Lambda function and cannot be used to update
-- the function configuration.
--
-- This operation requires permission for the 'lambda:UpdateFunctionCode'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionCode.html AWS API Reference> for UpdateFunctionCode.
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
    , uFunctionName

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateFunctionCode' smart constructor.
data UpdateFunctionCode = UpdateFunctionCode'
    { _uS3ObjectVersion :: !(Maybe Text)
    , _uS3Key           :: !(Maybe Text)
    , _uZipFile         :: !(Maybe Base64)
    , _uS3Bucket        :: !(Maybe Text)
    , _uFunctionName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFunctionCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uS3ObjectVersion'
--
-- * 'uS3Key'
--
-- * 'uZipFile'
--
-- * 'uS3Bucket'
--
-- * 'uFunctionName'
updateFunctionCode
    :: Text -- ^ 'uFunctionName'
    -> UpdateFunctionCode
updateFunctionCode pFunctionName_ =
    UpdateFunctionCode'
    { _uS3ObjectVersion = Nothing
    , _uS3Key = Nothing
    , _uZipFile = Nothing
    , _uS3Bucket = Nothing
    , _uFunctionName = pFunctionName_
    }

-- | The Amazon S3 object (the deployment package) version you want to
-- upload.
uS3ObjectVersion :: Lens' UpdateFunctionCode (Maybe Text)
uS3ObjectVersion = lens _uS3ObjectVersion (\ s a -> s{_uS3ObjectVersion = a});

-- | The Amazon S3 object (the deployment package) key name you want to
-- upload.
uS3Key :: Lens' UpdateFunctionCode (Maybe Text)
uS3Key = lens _uS3Key (\ s a -> s{_uS3Key = a});

-- | Based64-encoded .zip file containing your packaged source code.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
uZipFile :: Lens' UpdateFunctionCode (Maybe ByteString)
uZipFile = lens _uZipFile (\ s a -> s{_uZipFile = a}) . mapping _Base64;

-- | Amazon S3 bucket name where the .zip file containing your deployment
-- package is stored. This bucket must reside in the same AWS region where
-- you are creating the Lambda function.
uS3Bucket :: Lens' UpdateFunctionCode (Maybe Text)
uS3Bucket = lens _uS3Bucket (\ s a -> s{_uS3Bucket = a});

-- | The existing Lambda function name whose code you want to replace.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
uFunctionName :: Lens' UpdateFunctionCode Text
uFunctionName = lens _uFunctionName (\ s a -> s{_uFunctionName = a});

instance AWSRequest UpdateFunctionCode where
        type Sv UpdateFunctionCode = Lambda
        type Rs UpdateFunctionCode = FunctionConfiguration
        request = putJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateFunctionCode where
        toHeaders = const mempty

instance ToJSON UpdateFunctionCode where
        toJSON UpdateFunctionCode'{..}
          = object
              (catMaybes
                 [("S3ObjectVersion" .=) <$> _uS3ObjectVersion,
                  ("S3Key" .=) <$> _uS3Key,
                  ("ZipFile" .=) <$> _uZipFile,
                  ("S3Bucket" .=) <$> _uS3Bucket])

instance ToPath UpdateFunctionCode where
        toPath UpdateFunctionCode'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _uFunctionName,
               "/versions/HEAD/code"]

instance ToQuery UpdateFunctionCode where
        toQuery = const mempty
