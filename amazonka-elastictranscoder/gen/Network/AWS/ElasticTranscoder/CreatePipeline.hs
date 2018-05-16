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
-- Module      : Network.AWS.ElasticTranscoder.CreatePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePipeline operation creates a pipeline with settings that you specify.
--
--
module Network.AWS.ElasticTranscoder.CreatePipeline
    (
    -- * Creating a Request
      createPipeline
    , CreatePipeline
    -- * Request Lenses
    , cContentConfig
    , cOutputBucket
    , cAWSKMSKeyARN
    , cNotifications
    , cThumbnailConfig
    , cName
    , cInputBucket
    , cRole

    -- * Destructuring the Response
    , createPipelineResponse
    , CreatePipelineResponse
    -- * Response Lenses
    , crsWarnings
    , crsPipeline
    , crsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @CreatePipelineRequest@ structure.
--
--
--
-- /See:/ 'createPipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { _cContentConfig   :: !(Maybe PipelineOutputConfig)
  , _cOutputBucket    :: !(Maybe Text)
  , _cAWSKMSKeyARN    :: !(Maybe Text)
  , _cNotifications   :: !(Maybe Notifications)
  , _cThumbnailConfig :: !(Maybe PipelineOutputConfig)
  , _cName            :: !Text
  , _cInputBucket     :: !Text
  , _cRole            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cContentConfig' - The optional @ContentConfig@ object specifies information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists: which bucket to use, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files. If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ . If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ (Optional): The Permissions object specifies which users you want to have access to transcoded files and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.     * __Grantee Type__ : Specify the type of value that appears in the @Grantee@ object:      * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. For more information about canonical user IDs, see Access Control List (ACL) Overview in the Amazon Simple Storage Service Developer Guide. For more information about using CloudFront origin access identities to require that users use CloudFront URLs instead of Amazon S3 URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content. /Important:/ A canonical user ID is not the same as an AWS account number.     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * __Grantee__ : The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group      * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the files that Elastic Transcoder adds to the bucket, including playlists and video files. Valid values include:      * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- * 'cOutputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. (Use this, or use ContentConfig:Bucket plus ThumbnailConfig:Bucket.) Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.  /Important:/ When Elastic Transcoder saves files in @OutputBucket@ , it grants full control over the files only to the AWS account that owns the role that is specified by @Role@ .     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit @OutputBucket@ and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- * 'cAWSKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @AES-PKCS7@ , @AES-CTR@ , or @AES-GCM@ .
--
-- * 'cNotifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic. For more information, see Create a Topic in the Amazon Simple Notification Service Developer Guide.     * __Completed__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
-- * 'cThumbnailConfig' - The @ThumbnailConfig@ object specifies several values, including the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files. If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ even if you don't want to create thumbnails. If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.     * __Permissions__ (Optional): The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.     * __GranteeType__ : Specify the type of value that appears in the Grantee object:      * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.      * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * __Grantee__ : The AWS user or group that you want to have access to thumbnail files. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.      * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the thumbnail files that Elastic Transcoder adds to the bucket. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
--
-- * 'cName' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters.
--
-- * 'cInputBucket' - The Amazon S3 bucket in which you saved the media files that you want to transcode.
--
-- * 'cRole' - The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
createPipeline
    :: Text -- ^ 'cName'
    -> Text -- ^ 'cInputBucket'
    -> Text -- ^ 'cRole'
    -> CreatePipeline
createPipeline pName_ pInputBucket_ pRole_ =
  CreatePipeline'
    { _cContentConfig = Nothing
    , _cOutputBucket = Nothing
    , _cAWSKMSKeyARN = Nothing
    , _cNotifications = Nothing
    , _cThumbnailConfig = Nothing
    , _cName = pName_
    , _cInputBucket = pInputBucket_
    , _cRole = pRole_
    }


-- | The optional @ContentConfig@ object specifies information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists: which bucket to use, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files. If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ . If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ (Optional): The Permissions object specifies which users you want to have access to transcoded files and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.     * __Grantee Type__ : Specify the type of value that appears in the @Grantee@ object:      * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. For more information about canonical user IDs, see Access Control List (ACL) Overview in the Amazon Simple Storage Service Developer Guide. For more information about using CloudFront origin access identities to require that users use CloudFront URLs instead of Amazon S3 URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content. /Important:/ A canonical user ID is not the same as an AWS account number.     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.     * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * __Grantee__ : The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group      * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the files that Elastic Transcoder adds to the bucket, including playlists and video files. Valid values include:      * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
cContentConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cContentConfig = lens _cContentConfig (\ s a -> s{_cContentConfig = a})

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. (Use this, or use ContentConfig:Bucket plus ThumbnailConfig:Bucket.) Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.  /Important:/ When Elastic Transcoder saves files in @OutputBucket@ , it grants full control over the files only to the AWS account that owns the role that is specified by @Role@ .     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit @OutputBucket@ and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
cOutputBucket :: Lens' CreatePipeline (Maybe Text)
cOutputBucket = lens _cOutputBucket (\ s a -> s{_cOutputBucket = a})

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @AES-PKCS7@ , @AES-CTR@ , or @AES-GCM@ .
cAWSKMSKeyARN :: Lens' CreatePipeline (Maybe Text)
cAWSKMSKeyARN = lens _cAWSKMSKeyARN (\ s a -> s{_cAWSKMSKeyARN = a})

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic. For more information, see Create a Topic in the Amazon Simple Notification Service Developer Guide.     * __Completed__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition while processing a job in this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
cNotifications :: Lens' CreatePipeline (Maybe Notifications)
cNotifications = lens _cNotifications (\ s a -> s{_cNotifications = a})

-- | The @ThumbnailConfig@ object specifies several values, including the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files, which users you want to have access to the files, the type of access you want users to have, and the storage class that you want to assign to the files. If you specify values for @ContentConfig@ , you must also specify values for @ThumbnailConfig@ even if you don't want to create thumbnails. If you specify values for @ContentConfig@ and @ThumbnailConfig@ , omit the @OutputBucket@ object.     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.     * __Permissions__ (Optional): The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.     * __GranteeType__ : Specify the type of value that appears in the Grantee object:      * __Canonical__ : The value in the @Grantee@ object is either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * __Email__ : The value in the @Grantee@ object is the registered email address of an AWS account.      * __Group__ : The value in the @Grantee@ object is one of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * __Grantee__ : The AWS user or group that you want to have access to thumbnail files. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.      * __Access__ : The permission that you want to give to the AWS user that you specified in @Grantee@ . Permissions are granted on the thumbnail files that Elastic Transcoder adds to the bucket. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
cThumbnailConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cThumbnailConfig = lens _cThumbnailConfig (\ s a -> s{_cThumbnailConfig = a})

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters.
cName :: Lens' CreatePipeline Text
cName = lens _cName (\ s a -> s{_cName = a})

-- | The Amazon S3 bucket in which you saved the media files that you want to transcode.
cInputBucket :: Lens' CreatePipeline Text
cInputBucket = lens _cInputBucket (\ s a -> s{_cInputBucket = a})

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic Transcoder to use to create the pipeline.
cRole :: Lens' CreatePipeline Text
cRole = lens _cRole (\ s a -> s{_cRole = a})

instance AWSRequest CreatePipeline where
        type Rs CreatePipeline = CreatePipelineResponse
        request = postJSON elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' <$>
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure (fromEnum s)))

instance Hashable CreatePipeline where

instance NFData CreatePipeline where

instance ToHeaders CreatePipeline where
        toHeaders = const mempty

instance ToJSON CreatePipeline where
        toJSON CreatePipeline'{..}
          = object
              (catMaybes
                 [("ContentConfig" .=) <$> _cContentConfig,
                  ("OutputBucket" .=) <$> _cOutputBucket,
                  ("AwsKmsKeyArn" .=) <$> _cAWSKMSKeyARN,
                  ("Notifications" .=) <$> _cNotifications,
                  ("ThumbnailConfig" .=) <$> _cThumbnailConfig,
                  Just ("Name" .= _cName),
                  Just ("InputBucket" .= _cInputBucket),
                  Just ("Role" .= _cRole)])

instance ToPath CreatePipeline where
        toPath = const "/2012-09-25/pipelines"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
--
--
-- /See:/ 'createPipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { _crsWarnings       :: !(Maybe [Warning])
  , _crsPipeline       :: !(Maybe Pipeline)
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsWarnings' - Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline. Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- * 'crsPipeline' - A section of the response body that provides information about the pipeline that is created.
--
-- * 'crsResponseStatus' - -- | The response status code.
createPipelineResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreatePipelineResponse
createPipelineResponse pResponseStatus_ =
  CreatePipelineResponse'
    { _crsWarnings = Nothing
    , _crsPipeline = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline. Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
crsWarnings :: Lens' CreatePipelineResponse [Warning]
crsWarnings = lens _crsWarnings (\ s a -> s{_crsWarnings = a}) . _Default . _Coerce

-- | A section of the response body that provides information about the pipeline that is created.
crsPipeline :: Lens' CreatePipelineResponse (Maybe Pipeline)
crsPipeline = lens _crsPipeline (\ s a -> s{_crsPipeline = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreatePipelineResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreatePipelineResponse where
