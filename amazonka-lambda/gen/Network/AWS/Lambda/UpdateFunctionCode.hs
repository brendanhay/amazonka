{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Lambda.UpdateFunctionCode
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the code for the specified Lambda function. This operation must
-- only be used on an existing Lambda function and cannot be used to update
-- the function configuration.
--
-- This operation requires permission for the @lambda:UpdateFunctionCode@
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateFunctionCode.html>
module Network.AWS.Lambda.UpdateFunctionCode
    (
    -- * Request
      UpdateFunctionCode
    -- ** Request constructor
    , updateFunctionCode
    -- ** Request lenses
    , updS3ObjectVersion
    , updS3Key
    , updZipFile
    , updS3Bucket
    , updFunctionName

    -- * Response
    , FunctionConfiguration
    -- ** Response constructor
    , functionConfiguration
    -- ** Response lenses
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
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateFunctionCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'updS3ObjectVersion'
--
-- * 'updS3Key'
--
-- * 'updZipFile'
--
-- * 'updS3Bucket'
--
-- * 'updFunctionName'
data UpdateFunctionCode = UpdateFunctionCode'
    { _updS3ObjectVersion :: Maybe Text
    , _updS3Key           :: Maybe Text
    , _updZipFile         :: Maybe Base64
    , _updS3Bucket        :: Maybe Text
    , _updFunctionName    :: Text
    } deriving (Eq,Read,Show)

-- | 'UpdateFunctionCode' smart constructor.
updateFunctionCode :: Text -> UpdateFunctionCode
updateFunctionCode pFunctionName =
    UpdateFunctionCode'
    { _updS3ObjectVersion = Nothing
    , _updS3Key = Nothing
    , _updZipFile = Nothing
    , _updS3Bucket = Nothing
    , _updFunctionName = pFunctionName
    }

-- | The Amazon S3 object (the deployment package) version you want to
-- upload.
updS3ObjectVersion :: Lens' UpdateFunctionCode (Maybe Text)
updS3ObjectVersion = lens _updS3ObjectVersion (\ s a -> s{_updS3ObjectVersion = a});

-- | The Amazon S3 object (the deployment package) key name you want to
-- upload.
updS3Key :: Lens' UpdateFunctionCode (Maybe Text)
updS3Key = lens _updS3Key (\ s a -> s{_updS3Key = a});

-- | Based64-encoded .zip file containing your packaged source code.
updZipFile :: Lens' UpdateFunctionCode (Maybe Base64)
updZipFile = lens _updZipFile (\ s a -> s{_updZipFile = a});

-- | Amazon S3 bucket name where the .zip file containing your deployment
-- package is stored. This bucket must reside in the same AWS region where
-- you are creating the Lambda function.
updS3Bucket :: Lens' UpdateFunctionCode (Maybe Text)
updS3Bucket = lens _updS3Bucket (\ s a -> s{_updS3Bucket = a});

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
updFunctionName :: Lens' UpdateFunctionCode Text
updFunctionName = lens _updFunctionName (\ s a -> s{_updFunctionName = a});

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
              ["S3ObjectVersion" .= _updS3ObjectVersion,
               "S3Key" .= _updS3Key, "ZipFile" .= _updZipFile,
               "S3Bucket" .= _updS3Bucket]

instance ToPath UpdateFunctionCode where
        toPath UpdateFunctionCode'{..}
          = mconcat
              ["/2015-03-31/functions/", toText _updFunctionName,
               "/versions/HEAD/code"]

instance ToQuery UpdateFunctionCode where
        toQuery = const mempty
