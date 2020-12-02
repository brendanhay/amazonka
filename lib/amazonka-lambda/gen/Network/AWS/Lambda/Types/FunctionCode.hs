{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionCode where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The code for the Lambda function. You can specify either an object in Amazon S3, or upload a deployment package directly.
--
--
--
-- /See:/ 'functionCode' smart constructor.
data FunctionCode = FunctionCode'
  { _fcS3ObjectVersion ::
      !(Maybe Text),
    _fcS3Key :: !(Maybe Text),
    _fcZipFile :: !(Maybe (Sensitive Base64)),
    _fcS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcS3ObjectVersion' - For versioned objects, the version of the deployment package object to use.
--
-- * 'fcS3Key' - The Amazon S3 key of the deployment package.
--
-- * 'fcZipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'fcS3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
functionCode ::
  FunctionCode
functionCode =
  FunctionCode'
    { _fcS3ObjectVersion = Nothing,
      _fcS3Key = Nothing,
      _fcZipFile = Nothing,
      _fcS3Bucket = Nothing
    }

-- | For versioned objects, the version of the deployment package object to use.
fcS3ObjectVersion :: Lens' FunctionCode (Maybe Text)
fcS3ObjectVersion = lens _fcS3ObjectVersion (\s a -> s {_fcS3ObjectVersion = a})

-- | The Amazon S3 key of the deployment package.
fcS3Key :: Lens' FunctionCode (Maybe Text)
fcS3Key = lens _fcS3Key (\s a -> s {_fcS3Key = a})

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
fcZipFile :: Lens' FunctionCode (Maybe ByteString)
fcZipFile = lens _fcZipFile (\s a -> s {_fcZipFile = a}) . mapping (_Sensitive . _Base64)

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
fcS3Bucket :: Lens' FunctionCode (Maybe Text)
fcS3Bucket = lens _fcS3Bucket (\s a -> s {_fcS3Bucket = a})

instance Hashable FunctionCode

instance NFData FunctionCode

instance ToJSON FunctionCode where
  toJSON FunctionCode' {..} =
    object
      ( catMaybes
          [ ("S3ObjectVersion" .=) <$> _fcS3ObjectVersion,
            ("S3Key" .=) <$> _fcS3Key,
            ("ZipFile" .=) <$> _fcZipFile,
            ("S3Bucket" .=) <$> _fcS3Bucket
          ]
      )
