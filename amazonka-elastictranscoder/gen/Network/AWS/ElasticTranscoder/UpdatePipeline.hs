{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Use the @UpdatePipeline@ operation to update settings for a pipeline.
-- When you change pipeline settings, your changes take effect immediately.
-- Jobs that you have already submitted and that Elastic Transcoder has not
-- started to process are affected in addition to jobs that you submit
-- after you change settings.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/UpdatePipeline.html>
module Network.AWS.ElasticTranscoder.UpdatePipeline
    (
    -- * Request
      UpdatePipeline
    -- ** Request constructor
    , updatePipeline
    -- ** Request lenses
    , upInputBucket
    , upContentConfig
    , upRole
    , upName
    , upAWSKMSKeyARN
    , upThumbnailConfig
    , upNotifications
    , upId

    -- * Response
    , UpdatePipelineResponse
    -- ** Response constructor
    , updatePipelineResponse
    -- ** Response lenses
    , uprsWarnings
    , uprsPipeline
    , uprsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @UpdatePipelineRequest@ structure.
--
-- /See:/ 'updatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upInputBucket'
--
-- * 'upContentConfig'
--
-- * 'upRole'
--
-- * 'upName'
--
-- * 'upAWSKMSKeyARN'
--
-- * 'upThumbnailConfig'
--
-- * 'upNotifications'
--
-- * 'upId'
data UpdatePipeline = UpdatePipeline'
    { _upInputBucket     :: !(Maybe Text)
    , _upContentConfig   :: !(Maybe PipelineOutputConfig)
    , _upRole            :: !(Maybe Text)
    , _upName            :: !(Maybe Text)
    , _upAWSKMSKeyARN    :: !(Maybe Text)
    , _upThumbnailConfig :: !(Maybe PipelineOutputConfig)
    , _upNotifications   :: !(Maybe Notifications)
    , _upId              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipeline' smart constructor.
updatePipeline :: Text -> UpdatePipeline
updatePipeline pId_ =
    UpdatePipeline'
    { _upInputBucket = Nothing
    , _upContentConfig = Nothing
    , _upRole = Nothing
    , _upName = Nothing
    , _upAWSKMSKeyARN = Nothing
    , _upThumbnailConfig = Nothing
    , _upNotifications = Nothing
    , _upId = pId_
    }

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode and the graphics that you want to use as watermarks.
upInputBucket :: Lens' UpdatePipeline (Maybe Text)
upInputBucket = lens _upInputBucket (\ s a -> s{_upInputBucket = a});

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
upContentConfig :: Lens' UpdatePipeline (Maybe PipelineOutputConfig)
upContentConfig = lens _upContentConfig (\ s a -> s{_upContentConfig = a});

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to transcode jobs for this pipeline.
upRole :: Lens' UpdatePipeline (Maybe Text)
upRole = lens _upRole (\ s a -> s{_upRole = a});

-- | The name of the pipeline. We recommend that the name be unique within
-- the AWS account, but uniqueness is not enforced.
--
-- Constraints: Maximum 40 characters
upName :: Lens' UpdatePipeline (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a});

-- | The AWS Key Management Service (AWS KMS) key that you want to use with
-- this pipeline.
--
-- If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@, you
-- don\'t need to provide a key with your job because a default key, known
-- as an AWS-KMS key, is created for you automatically. You need to provide
-- an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if
-- you are using an @Encryption:Mode@ of @AES-PKCS7@, @AES-CTR@, or
-- @AES-GCM@.
upAWSKMSKeyARN :: Lens' UpdatePipeline (Maybe Text)
upAWSKMSKeyARN = lens _upAWSKMSKeyARN (\ s a -> s{_upAWSKMSKeyARN = a});

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
upThumbnailConfig :: Lens' UpdatePipeline (Maybe PipelineOutputConfig)
upThumbnailConfig = lens _upThumbnailConfig (\ s a -> s{_upThumbnailConfig = a});

-- | FIXME: Undocumented member.
upNotifications :: Lens' UpdatePipeline (Maybe Notifications)
upNotifications = lens _upNotifications (\ s a -> s{_upNotifications = a});

-- | The ID of the pipeline that you want to update.
upId :: Lens' UpdatePipeline Text
upId = lens _upId (\ s a -> s{_upId = a});

instance AWSRequest UpdatePipeline where
        type Sv UpdatePipeline = ElasticTranscoder
        type Rs UpdatePipeline = UpdatePipelineResponse
        request = putJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineResponse' <$>
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure (fromEnum s)))

instance ToHeaders UpdatePipeline where
        toHeaders = const mempty

instance ToJSON UpdatePipeline where
        toJSON UpdatePipeline'{..}
          = object
              ["InputBucket" .= _upInputBucket,
               "ContentConfig" .= _upContentConfig,
               "Role" .= _upRole, "Name" .= _upName,
               "AwsKmsKeyArn" .= _upAWSKMSKeyARN,
               "ThumbnailConfig" .= _upThumbnailConfig,
               "Notifications" .= _upNotifications]

instance ToPath UpdatePipeline where
        toPath UpdatePipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toPath _upId]

instance ToQuery UpdatePipeline where
        toQuery = const mempty

-- | When you update a pipeline, Elastic Transcoder returns the values that
-- you specified in the request.
--
-- /See:/ 'updatePipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprsWarnings'
--
-- * 'uprsPipeline'
--
-- * 'uprsStatus'
data UpdatePipelineResponse = UpdatePipelineResponse'
    { _uprsWarnings :: !(Maybe [Warning])
    , _uprsPipeline :: !(Maybe Pipeline)
    , _uprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineResponse' smart constructor.
updatePipelineResponse :: Int -> UpdatePipelineResponse
updatePipelineResponse pStatus_ =
    UpdatePipelineResponse'
    { _uprsWarnings = Nothing
    , _uprsPipeline = Nothing
    , _uprsStatus = pStatus_
    }

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
uprsWarnings :: Lens' UpdatePipelineResponse [Warning]
uprsWarnings = lens _uprsWarnings (\ s a -> s{_uprsWarnings = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
uprsPipeline :: Lens' UpdatePipelineResponse (Maybe Pipeline)
uprsPipeline = lens _uprsPipeline (\ s a -> s{_uprsPipeline = a});

-- | FIXME: Undocumented member.
uprsStatus :: Lens' UpdatePipelineResponse Int
uprsStatus = lens _uprsStatus (\ s a -> s{_uprsStatus = a});
