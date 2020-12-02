{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.S3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.S3Storage where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the storage parameters for S3 and S3 buckets for an instance store-backed AMI.
--
--
--
-- /See:/ 's3Storage' smart constructor.
data S3Storage = S3Storage'
  { _ssPrefix :: !(Maybe Text),
    _ssUploadPolicy :: !(Maybe Base64),
    _ssBucket :: !(Maybe Text),
    _ssUploadPolicySignature :: !(Maybe Text),
    _ssAWSAccessKeyId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssPrefix' - The beginning of the file name of the AMI.
--
-- * 'ssUploadPolicy' - An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'ssBucket' - The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- * 'ssUploadPolicySignature' - The signature of the JSON document.
--
-- * 'ssAWSAccessKeyId' - The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
s3Storage ::
  S3Storage
s3Storage =
  S3Storage'
    { _ssPrefix = Nothing,
      _ssUploadPolicy = Nothing,
      _ssBucket = Nothing,
      _ssUploadPolicySignature = Nothing,
      _ssAWSAccessKeyId = Nothing
    }

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\s a -> s {_ssPrefix = a})

-- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ssUploadPolicy :: Lens' S3Storage (Maybe ByteString)
ssUploadPolicy = lens _ssUploadPolicy (\s a -> s {_ssUploadPolicy = a}) . mapping _Base64

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\s a -> s {_ssBucket = a})

-- | The signature of the JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature = lens _ssUploadPolicySignature (\s a -> s {_ssUploadPolicySignature = a})

-- | The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\s a -> s {_ssAWSAccessKeyId = a})

instance FromXML S3Storage where
  parseXML x =
    S3Storage'
      <$> (x .@? "prefix")
      <*> (x .@? "uploadPolicy")
      <*> (x .@? "bucket")
      <*> (x .@? "uploadPolicySignature")
      <*> (x .@? "AWSAccessKeyId")

instance Hashable S3Storage

instance NFData S3Storage

instance ToQuery S3Storage where
  toQuery S3Storage' {..} =
    mconcat
      [ "Prefix" =: _ssPrefix,
        "UploadPolicy" =: _ssUploadPolicy,
        "Bucket" =: _ssBucket,
        "UploadPolicySignature" =: _ssUploadPolicySignature,
        "AWSAccessKeyId" =: _ssAWSAccessKeyId
      ]
