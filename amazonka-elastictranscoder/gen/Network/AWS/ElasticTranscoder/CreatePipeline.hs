{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.CreatePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreatePipeline operation creates a pipeline with settings that you
-- specify. POST /2012-09-25/pipelines HTTP/1.1 Content-Type:
-- application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Name":"Default",
-- "InputBucket":"salesoffice.example.com-source",
-- "OutputBucket":"salesoffice.example.com-public-promos",
-- "Role":"arn:aws:iam::123456789012:role/transcode-service",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" } "ContentConfig":{
-- "Bucket": "My-S3-bucket", "Permissions":[ { "GranteeType":"Email",
-- "Grantee": "marketing-promos@example.com", "Access":[ "Read" ] } ],
-- "StorageClass":"Standard" }, "ThumbnailConfig":{ "Bucket":"My-S3-bucket",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos@example.com", "Access":[ "Read" ] } ],
-- "StorageClass":"Standard" } } Status: 201 Created x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Pipeline":{ Id":"1111111111111-abcde1",
-- "InputBucket":"salesoffice.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ET_Errors", "Progressing":"",
-- "Warning":"" }, "ContentConfig":{
-- "Bucket":"salesoffice.example.com-public-promos", "Permissions":[ {
-- "GranteeType":"Email", "Grantee":"marketing-promos@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"Standard" }, "ThumbnailConfig":{
-- "Bucket":"salesoffice.example.com-public-promos-thumbnails",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos@example.com", "Access":[ "FullControl" ] } ],
-- "StorageClass":"ReducedRedundancy" }, "Status":"Active" } }.
module Network.AWS.ElasticTranscoder.CreatePipeline
    (
    -- * Request
      CreatePipeline
    -- ** Request constructor
    , createPipeline
    -- ** Request lenses
    , cpName
    , cpInputBucket
    , cpOutputBucket
    , cpRole
    , cpNotifications
    , cpContentConfig
    , cpThumbnailConfig

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , cprPipeline
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The CreatePipelineRequest structure.
data CreatePipeline = CreatePipeline
    { _cpName :: Text
    , _cpInputBucket :: Text
    , _cpOutputBucket :: Maybe Text
    , _cpRole :: Text
    , _cpNotifications :: Maybe Notifications
    , _cpContentConfig :: Maybe PipelineOutputConfig
    , _cpThumbnailConfig :: Maybe PipelineOutputConfig
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePipeline' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @InputBucket ::@ @Text@
--
-- * @OutputBucket ::@ @Maybe Text@
--
-- * @Role ::@ @Text@
--
-- * @Notifications ::@ @Maybe Notifications@
--
-- * @ContentConfig ::@ @Maybe PipelineOutputConfig@
--
-- * @ThumbnailConfig ::@ @Maybe PipelineOutputConfig@
--
createPipeline :: Text -- ^ 'cpName'
               -> Text -- ^ 'cpInputBucket'
               -> Text -- ^ 'cpRole'
               -> CreatePipeline
createPipeline p1 p2 p4 = CreatePipeline
    { _cpName = p1
    , _cpInputBucket = p2
    , _cpOutputBucket = Nothing
    , _cpRole = p4
    , _cpNotifications = Nothing
    , _cpContentConfig = Nothing
    , _cpThumbnailConfig = Nothing
    }

-- | The name of the pipeline. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced. Constraints: Maximum 40
-- characters.
cpName :: Lens' CreatePipeline Text
cpName = lens _cpName (\s a -> s { _cpName = a })

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode.
cpInputBucket :: Lens' CreatePipeline Text
cpInputBucket = lens _cpInputBucket (\s a -> s { _cpInputBucket = a })

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. (Use this, or use ContentConfig:Bucket plus
-- ThumbnailConfig:Bucket.) Specify this value when all of the following are
-- true: You want to save transcoded files, thumbnails (if any), and playlists
-- (if any) together in one bucket. You do not want to specify the users or
-- groups who have access to the transcoded files, thumbnails, and playlists.
-- You do not want to specify the permissions that Elastic Transcoder grants
-- to the files. When Elastic Transcoder saves files in OutputBucket, it
-- grants full control over the files only to the AWS account that owns the
-- role that is specified by Role. You want to associate the transcoded files
-- and thumbnails with the Amazon S3 Standard storage class. If you want to
-- save transcoded files and playlists in one bucket and thumbnails in another
-- bucket, specify which users can access the transcoded files or the
-- permissions the users have, or change the Amazon S3 storage class, omit
-- OutputBucket and specify values for ContentConfig and ThumbnailConfig
-- instead.
cpOutputBucket :: Lens' CreatePipeline (Maybe Text)
cpOutputBucket = lens _cpOutputBucket (\s a -> s { _cpOutputBucket = a })

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to create the pipeline.
cpRole :: Lens' CreatePipeline Text
cpRole = lens _cpRole (\s a -> s { _cpRole = a })

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to
-- notify to report job status. To receive notifications, you must also
-- subscribe to the new topic in the Amazon SNS console. Progressing: The
-- topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic
-- that you want to notify when Elastic Transcoder has started to process a
-- job in this pipeline. This is the ARN that Amazon SNS returned when you
-- created the topic. For more information, see Create a Topic in the Amazon
-- Simple Notification Service Developer Guide. Completed: The topic ARN for
-- the Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing a job in this pipeline. This is the ARN that Amazon SNS
-- returned when you created the topic. Warning: The topic ARN for the Amazon
-- SNS topic that you want to notify when Elastic Transcoder encounters a
-- warning condition while processing a job in this pipeline. This is the ARN
-- that Amazon SNS returned when you created the topic. Error: The topic ARN
-- for the Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition while processing a job in this pipeline. This
-- is the ARN that Amazon SNS returned when you created the topic.
cpNotifications :: Lens' CreatePipeline (Maybe Notifications)
cpNotifications = lens _cpNotifications (\s a -> s { _cpNotifications = a })

-- | The optional ContentConfig object specifies information about the Amazon S3
-- bucket in which you want Elastic Transcoder to save transcoded files and
-- playlists: which bucket to use, which users you want to have access to the
-- files, the type of access you want users to have, and the storage class
-- that you want to assign to the files. If you specify values for
-- ContentConfig, you must also specify values for ThumbnailConfig. If you
-- specify values for ContentConfig and ThumbnailConfig, omit the OutputBucket
-- object. Bucket: The Amazon S3 bucket in which you want Elastic Transcoder
-- to save transcoded files and playlists. Permissions (Optional): The
-- Permissions object specifies which users you want to have access to
-- transcoded files and the type of access you want them to have. You can
-- grant permissions to a maximum of 30 users and/or predefined Amazon S3
-- groups. Grantee Type: Specify the type of value that appears in the Grantee
-- object: Canonical: The value in the Grantee object is either the canonical
-- user ID for an AWS account or an origin access identity for an Amazon
-- CloudFront distribution. For more information about canonical user IDs, see
-- Access Control List (ACL) Overview in the Amazon Simple Storage Service
-- Developer Guide. For more information about using CloudFront origin access
-- identities to require that users use CloudFront URLs instead of Amazon S3
-- URLs, see Using an Origin Access Identity to Restrict Access to Your Amazon
-- S3 Content. A canonical user ID is not the same as an AWS account number.
-- Email: The value in the Grantee object is the registered email address of
-- an AWS account. Group: The value in the Grantee object is one of the
-- following predefined Amazon S3 groups: AllUsers, AuthenticatedUsers, or
-- LogDelivery. Grantee: The AWS user or group that you want to have access to
-- transcoded files and playlists. To identify the user or group, you can
-- specify the canonical user ID for an AWS account, an origin access identity
-- for a CloudFront distribution, the registered email address of an AWS
-- account, or a predefined Amazon S3 group Access: The permission that you
-- want to give to the AWS user that you specified in Grantee. Permissions are
-- granted on the files that Elastic Transcoder adds to the bucket, including
-- playlists and video files. Valid values include: READ: The grantee can read
-- the objects and metadata for objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. READ_ACP: The grantee can read the object ACL for objects
-- that Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The
-- grantee can write the ACL for the objects that Elastic Transcoder adds to
-- the Amazon S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and
-- WRITE_ACP permissions for the objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. StorageClass: The Amazon S3 storage class, Standard or
-- ReducedRedundancy, that you want Elastic Transcoder to assign to the video
-- files and playlists that it stores in your Amazon S3 bucket.
cpContentConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cpContentConfig = lens _cpContentConfig (\s a -> s { _cpContentConfig = a })

-- | The ThumbnailConfig object specifies several values, including the Amazon
-- S3 bucket in which you want Elastic Transcoder to save thumbnail files,
-- which users you want to have access to the files, the type of access you
-- want users to have, and the storage class that you want to assign to the
-- files. If you specify values for ContentConfig, you must also specify
-- values for ThumbnailConfig even if you don't want to create thumbnails. If
-- you specify values for ContentConfig and ThumbnailConfig, omit the
-- OutputBucket object. Bucket: The Amazon S3 bucket in which you want Elastic
-- Transcoder to save thumbnail files. Permissions (Optional): The Permissions
-- object specifies which users and/or predefined Amazon S3 groups you want to
-- have access to thumbnail files, and the type of access you want them to
-- have. You can grant permissions to a maximum of 30 users and/or predefined
-- Amazon S3 groups. GranteeType: Specify the type of value that appears in
-- the Grantee object: Canonical: The value in the Grantee object is either
-- the canonical user ID for an AWS account or an origin access identity for
-- an Amazon CloudFront distribution. A canonical user ID is not the same as
-- an AWS account number. Email: The value in the Grantee object is the
-- registered email address of an AWS account. Group: The value in the Grantee
-- object is one of the following predefined Amazon S3 groups: AllUsers,
-- AuthenticatedUsers, or LogDelivery. Grantee: The AWS user or group that you
-- want to have access to thumbnail files. To identify the user or group, you
-- can specify the canonical user ID for an AWS account, an origin access
-- identity for a CloudFront distribution, the registered email address of an
-- AWS account, or a predefined Amazon S3 group. Access: The permission that
-- you want to give to the AWS user that you specified in Grantee. Permissions
-- are granted on the thumbnail files that Elastic Transcoder adds to the
-- bucket. Valid values include: READ: The grantee can read the thumbnails and
-- metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.
-- READ_ACP: The grantee can read the object ACL for thumbnails that Elastic
-- Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The grantee can write
-- the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3
-- bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and WRITE_ACP
-- permissions for the thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket. StorageClass: The Amazon S3 storage class, Standard or
-- ReducedRedundancy, that you want Elastic Transcoder to assign to the
-- thumbnails that it stores in your Amazon S3 bucket.
cpThumbnailConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cpThumbnailConfig =
    lens _cpThumbnailConfig (\s a -> s { _cpThumbnailConfig = a })

instance ToPath CreatePipeline

instance ToQuery CreatePipeline

instance ToHeaders CreatePipeline

instance ToJSON CreatePipeline

-- | When you create a pipeline, Elastic Transcoder returns the values that you
-- specified in the request.
newtype CreatePipelineResponse = CreatePipelineResponse
    { _cprPipeline :: Maybe Pipeline
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePipelineResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Pipeline ::@ @Maybe Pipeline@
--
createPipelineResponse :: CreatePipelineResponse
createPipelineResponse = CreatePipelineResponse
    { _cprPipeline = Nothing
    }

-- | A section of the response body that provides information about the pipeline
-- that is created.
cprPipeline :: Lens' CreatePipelineResponse (Maybe Pipeline)
cprPipeline = lens _cprPipeline (\s a -> s { _cprPipeline = a })

instance FromJSON CreatePipelineResponse

instance AWSRequest CreatePipeline where
    type Sv CreatePipeline = ElasticTranscoder
    type Rs CreatePipeline = CreatePipelineResponse

    request = get
    response _ = jsonResponse
