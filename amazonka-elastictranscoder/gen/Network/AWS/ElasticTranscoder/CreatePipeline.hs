{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.CreatePipeline
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

-- | The CreatePipeline operation creates a pipeline with settings that you
-- specify.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CreatePipeline.html>
module Network.AWS.ElasticTranscoder.CreatePipeline
    (
    -- * Request
      CreatePipeline
    -- ** Request constructor
    , createPipeline
    -- ** Request lenses
    , creContentConfig
    , creOutputBucket
    , creAWSKMSKeyARN
    , creThumbnailConfig
    , creNotifications
    , creName
    , creInputBucket
    , creRole

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , creWarnings
    , crePipeline
    , creStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @CreatePipelineRequest@ structure.
--
-- /See:/ 'createPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creContentConfig'
--
-- * 'creOutputBucket'
--
-- * 'creAWSKMSKeyARN'
--
-- * 'creThumbnailConfig'
--
-- * 'creNotifications'
--
-- * 'creName'
--
-- * 'creInputBucket'
--
-- * 'creRole'
data CreatePipeline = CreatePipeline'
    { _creContentConfig   :: !(Maybe PipelineOutputConfig)
    , _creOutputBucket    :: !(Maybe Text)
    , _creAWSKMSKeyARN    :: !(Maybe Text)
    , _creThumbnailConfig :: !(Maybe PipelineOutputConfig)
    , _creNotifications   :: !(Maybe Notifications)
    , _creName            :: !Text
    , _creInputBucket     :: !Text
    , _creRole            :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreatePipeline' smart constructor.
createPipeline :: Text -> Text -> Text -> CreatePipeline
createPipeline pName pInputBucket pRole =
    CreatePipeline'
    { _creContentConfig = Nothing
    , _creOutputBucket = Nothing
    , _creAWSKMSKeyARN = Nothing
    , _creThumbnailConfig = Nothing
    , _creNotifications = Nothing
    , _creName = pName
    , _creInputBucket = pInputBucket
    , _creRole = pRole
    }

-- | The optional @ContentConfig@ object specifies information about the
-- Amazon S3 bucket in which you want Elastic Transcoder to save transcoded
-- files and playlists: which bucket to use, which users you want to have
-- access to the files, the type of access you want users to have, and the
-- storage class that you want to assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save transcoded files and playlists.
-- -   __Permissions__ (Optional): The Permissions object specifies which
--     users you want to have access to transcoded files and the type of
--     access you want them to have. You can grant permissions to a maximum
--     of 30 users and\/or predefined Amazon S3 groups.
-- -   __Grantee Type__: Specify the type of value that appears in the
--     @Grantee@ object:
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution. For more
--         information about canonical user IDs, see Access Control List
--         (ACL) Overview in the Amazon Simple Storage Service Developer
--         Guide. For more information about using CloudFront origin access
--         identities to require that users use CloudFront URLs instead of
--         Amazon S3 URLs, see Using an Origin Access Identity to Restrict
--         Access to Your Amazon S3 Content.
--         A canonical user ID is not the same as an AWS account number.
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
-- -   __Grantee__: The AWS user or group that you want to have access to
--     transcoded files and playlists. To identify the user or group, you
--     can specify the canonical user ID for an AWS account, an origin
--     access identity for a CloudFront distribution, the registered email
--     address of an AWS account, or a predefined Amazon S3 group
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     files that Elastic Transcoder adds to the bucket, including
--     playlists and video files. Valid values include:
--     -   @READ@: The grantee can read the objects and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @READ_ACP@: The grantee can read the object ACL for objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @WRITE_ACP@: The grantee can write the ACL for the objects that
--         Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the objects that Elastic Transcoder
--         adds to the Amazon S3 bucket.
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the video files and playlists that it stores in your Amazon S3
--     bucket.
creContentConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
creContentConfig = lens _creContentConfig (\ s a -> s{_creContentConfig = a});

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. (Use this, or use ContentConfig:Bucket plus
-- ThumbnailConfig:Bucket.)
--
-- Specify this value when all of the following are true:
--
-- -   You want to save transcoded files, thumbnails (if any), and
--     playlists (if any) together in one bucket.
-- -   You do not want to specify the users or groups who have access to
--     the transcoded files, thumbnails, and playlists.
-- -   You do not want to specify the permissions that Elastic Transcoder
--     grants to the files.
--     When Elastic Transcoder saves files in @OutputBucket@, it grants
--     full control over the files only to the AWS account that owns the
--     role that is specified by @Role@.
-- -   You want to associate the transcoded files and thumbnails with the
--     Amazon S3 Standard storage class.
--
-- If you want to save transcoded files and playlists in one bucket and
-- thumbnails in another bucket, specify which users can access the
-- transcoded files or the permissions the users have, or change the Amazon
-- S3 storage class, omit @OutputBucket@ and specify values for
-- @ContentConfig@ and @ThumbnailConfig@ instead.
creOutputBucket :: Lens' CreatePipeline (Maybe Text)
creOutputBucket = lens _creOutputBucket (\ s a -> s{_creOutputBucket = a});

-- | The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @AES-PKCS7@, @AES-CTR@, or
-- @AES-GCM@.
creAWSKMSKeyARN :: Lens' CreatePipeline (Maybe Text)
creAWSKMSKeyARN = lens _creAWSKMSKeyARN (\ s a -> s{_creAWSKMSKeyARN = a});

-- | The @ThumbnailConfig@ object specifies several values, including the
-- Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail
-- files, which users you want to have access to the files, the type of
-- access you want users to have, and the storage class that you want to
-- assign to the files.
--
-- If you specify values for @ContentConfig@, you must also specify values
-- for @ThumbnailConfig@ even if you don\'t want to create thumbnails.
--
-- If you specify values for @ContentConfig@ and @ThumbnailConfig@, omit
-- the @OutputBucket@ object.
--
-- -   __Bucket__: The Amazon S3 bucket in which you want Elastic
--     Transcoder to save thumbnail files.
-- -   __Permissions__ (Optional): The @Permissions@ object specifies which
--     users and\/or predefined Amazon S3 groups you want to have access to
--     thumbnail files, and the type of access you want them to have. You
--     can grant permissions to a maximum of 30 users and\/or predefined
--     Amazon S3 groups.
-- -   __GranteeType__: Specify the type of value that appears in the
--     Grantee object:
--     -   __Canonical__: The value in the @Grantee@ object is either the
--         canonical user ID for an AWS account or an origin access
--         identity for an Amazon CloudFront distribution.
--         A canonical user ID is not the same as an AWS account number.
--     -   __Email__: The value in the @Grantee@ object is the registered
--         email address of an AWS account.
--     -   __Group__: The value in the @Grantee@ object is one of the
--         following predefined Amazon S3 groups: @AllUsers@,
--         @AuthenticatedUsers@, or @LogDelivery@.
-- -   __Grantee__: The AWS user or group that you want to have access to
--     thumbnail files. To identify the user or group, you can specify the
--     canonical user ID for an AWS account, an origin access identity for
--     a CloudFront distribution, the registered email address of an AWS
--     account, or a predefined Amazon S3 group.
-- -   __Access__: The permission that you want to give to the AWS user
--     that you specified in @Grantee@. Permissions are granted on the
--     thumbnail files that Elastic Transcoder adds to the bucket. Valid
--     values include:
--     -   @READ@: The grantee can read the thumbnails and metadata for
--         objects that Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @READ_ACP@: The grantee can read the object ACL for thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails
--         that Elastic Transcoder adds to the Amazon S3 bucket.
--     -   @FULL_CONTROL@: The grantee has @READ@, @READ_ACP@, and
--         @WRITE_ACP@ permissions for the thumbnails that Elastic
--         Transcoder adds to the Amazon S3 bucket.
-- -   __StorageClass__: The Amazon S3 storage class, @Standard@ or
--     @ReducedRedundancy@, that you want Elastic Transcoder to assign to
--     the thumbnails that it stores in your Amazon S3 bucket.
creThumbnailConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
creThumbnailConfig = lens _creThumbnailConfig (\ s a -> s{_creThumbnailConfig = a});

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process a job in this pipeline. This is
--     the ARN that Amazon SNS returned when you created the topic. For
--     more information, see Create a Topic in the Amazon Simple
--     Notification Service Developer Guide.
-- -   __Completed__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job in
--     this pipeline. This is the ARN that Amazon SNS returned when you
--     created the topic.
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition while
--     processing a job in this pipeline. This is the ARN that Amazon SNS
--     returned when you created the topic.
creNotifications :: Lens' CreatePipeline (Maybe Notifications)
creNotifications = lens _creNotifications (\ s a -> s{_creNotifications = a});

-- | The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters.
creName :: Lens' CreatePipeline Text
creName = lens _creName (\ s a -> s{_creName = a});

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode.
creInputBucket :: Lens' CreatePipeline Text
creInputBucket = lens _creInputBucket (\ s a -> s{_creInputBucket = a});

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to create the pipeline.
creRole :: Lens' CreatePipeline Text
creRole = lens _creRole (\ s a -> s{_creRole = a});

instance AWSRequest CreatePipeline where
        type Sv CreatePipeline = ElasticTranscoder
        type Rs CreatePipeline = CreatePipelineResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' <$>
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure s))

instance ToHeaders CreatePipeline where
        toHeaders = const mempty

instance ToJSON CreatePipeline where
        toJSON CreatePipeline'{..}
          = object
              ["ContentConfig" .= _creContentConfig,
               "OutputBucket" .= _creOutputBucket,
               "AwsKmsKeyArn" .= _creAWSKMSKeyARN,
               "ThumbnailConfig" .= _creThumbnailConfig,
               "Notifications" .= _creNotifications,
               "Name" .= _creName, "InputBucket" .= _creInputBucket,
               "Role" .= _creRole]

instance ToPath CreatePipeline where
        toPath = const "/2012-09-25/pipelines"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | When you create a pipeline, Elastic Transcoder returns the values that
-- you specified in the request.
--
-- /See:/ 'createPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creWarnings'
--
-- * 'crePipeline'
--
-- * 'creStatus'
data CreatePipelineResponse = CreatePipelineResponse'
    { _creWarnings :: !(Maybe [Warning])
    , _crePipeline :: !(Maybe Pipeline)
    , _creStatus   :: !Status
    } deriving (Eq,Show)

-- | 'CreatePipelineResponse' smart constructor.
createPipelineResponse :: Status -> CreatePipelineResponse
createPipelineResponse pStatus =
    CreatePipelineResponse'
    { _creWarnings = Nothing
    , _crePipeline = Nothing
    , _creStatus = pStatus
    }

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
creWarnings :: Lens' CreatePipelineResponse [Warning]
creWarnings = lens _creWarnings (\ s a -> s{_creWarnings = a}) . _Default;

-- | A section of the response body that provides information about the
-- pipeline that is created.
crePipeline :: Lens' CreatePipelineResponse (Maybe Pipeline)
crePipeline = lens _crePipeline (\ s a -> s{_crePipeline = a});

-- | FIXME: Undocumented member.
creStatus :: Lens' CreatePipelineResponse Status
creStatus = lens _creStatus (\ s a -> s{_creStatus = a});
