{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Pipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Pipeline where

import Network.AWS.ElasticTranscoder.Types.Notifications
import Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The pipeline (queue) that is used to manage jobs.
--
--
--
-- /See:/ 'pipeline' smart constructor.
data Pipeline = Pipeline'
  { _pipStatus :: !(Maybe Text),
    _pipARN :: !(Maybe Text),
    _pipInputBucket :: !(Maybe Text),
    _pipContentConfig :: !(Maybe PipelineOutputConfig),
    _pipOutputBucket :: !(Maybe Text),
    _pipRole :: !(Maybe Text),
    _pipName :: !(Maybe Text),
    _pipAWSKMSKeyARN :: !(Maybe Text),
    _pipId :: !(Maybe Text),
    _pipNotifications :: !(Maybe Notifications),
    _pipThumbnailConfig :: !(Maybe PipelineOutputConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Pipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipStatus' - The current status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
--
-- * 'pipARN' - The Amazon Resource Name (ARN) for the pipeline.
--
-- * 'pipInputBucket' - The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
--
-- * 'pipContentConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the @Grantee@ object:      * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- * 'pipOutputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
--
-- * 'pipRole' - The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
--
-- * 'pipName' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters
--
-- * 'pipAWSKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
--
-- * 'pipId' - The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
--
-- * 'pipNotifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.     * __Complete__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- * 'pipThumbnailConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.      * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
pipeline ::
  Pipeline
pipeline =
  Pipeline'
    { _pipStatus = Nothing,
      _pipARN = Nothing,
      _pipInputBucket = Nothing,
      _pipContentConfig = Nothing,
      _pipOutputBucket = Nothing,
      _pipRole = Nothing,
      _pipName = Nothing,
      _pipAWSKMSKeyARN = Nothing,
      _pipId = Nothing,
      _pipNotifications = Nothing,
      _pipThumbnailConfig = Nothing
    }

-- | The current status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
pipStatus :: Lens' Pipeline (Maybe Text)
pipStatus = lens _pipStatus (\s a -> s {_pipStatus = a})

-- | The Amazon Resource Name (ARN) for the pipeline.
pipARN :: Lens' Pipeline (Maybe Text)
pipARN = lens _pipARN (\s a -> s {_pipARN = a})

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
pipInputBucket :: Lens' Pipeline (Maybe Text)
pipInputBucket = lens _pipInputBucket (\s a -> s {_pipInputBucket = a})

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the @Grantee@ object:      * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
pipContentConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pipContentConfig = lens _pipContentConfig (\s a -> s {_pipContentConfig = a})

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
pipOutputBucket :: Lens' Pipeline (Maybe Text)
pipOutputBucket = lens _pipOutputBucket (\s a -> s {_pipOutputBucket = a})

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
pipRole :: Lens' Pipeline (Maybe Text)
pipRole = lens _pipRole (\s a -> s {_pipRole = a})

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters
pipName :: Lens' Pipeline (Maybe Text)
pipName = lens _pipName (\s a -> s {_pipName = a})

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @s3@ or @s3-aws-kms@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @aes-cbc-pkcs7@ , @aes-ctr@ , or @aes-gcm@ .
pipAWSKMSKeyARN :: Lens' Pipeline (Maybe Text)
pipAWSKMSKeyARN = lens _pipAWSKMSKeyARN (\s a -> s {_pipAWSKMSKeyARN = a})

-- | The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
pipId :: Lens' Pipeline (Maybe Text)
pipId = lens _pipId (\s a -> s {_pipId = a})

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.     * __Complete__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
pipNotifications :: Lens' Pipeline (Maybe Notifications)
pipNotifications = lens _pipNotifications (\s a -> s {_pipNotifications = a})

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.      * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
pipThumbnailConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pipThumbnailConfig = lens _pipThumbnailConfig (\s a -> s {_pipThumbnailConfig = a})

instance FromJSON Pipeline where
  parseJSON =
    withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            <$> (x .:? "Status")
            <*> (x .:? "Arn")
            <*> (x .:? "InputBucket")
            <*> (x .:? "ContentConfig")
            <*> (x .:? "OutputBucket")
            <*> (x .:? "Role")
            <*> (x .:? "Name")
            <*> (x .:? "AwsKmsKeyArn")
            <*> (x .:? "Id")
            <*> (x .:? "Notifications")
            <*> (x .:? "ThumbnailConfig")
      )

instance Hashable Pipeline

instance NFData Pipeline
