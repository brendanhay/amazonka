{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- specify.
module Network.AWS.ElasticTranscoder.CreatePipeline
    (
    -- * Request
      CreatePipeline
    -- ** Request constructor
    , createPipeline
    -- ** Request lenses
    , cp1ContentConfig
    , cp1InputBucket
    , cp1Name
    , cp1Notifications
    , cp1OutputBucket
    , cp1Role
    , cp1ThumbnailConfig

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , cprPipeline
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

data CreatePipeline = CreatePipeline
    { _cp1ContentConfig   :: Maybe PipelineOutputConfig
    , _cp1InputBucket     :: Text
    , _cp1Name            :: Text
    , _cp1Notifications   :: Maybe Notifications
    , _cp1OutputBucket    :: Maybe Text
    , _cp1Role            :: Text
    , _cp1ThumbnailConfig :: Maybe PipelineOutputConfig
    } deriving (Eq, Show, Generic)

-- | 'CreatePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cp1ContentConfig' @::@ 'Maybe' 'PipelineOutputConfig'
--
-- * 'cp1InputBucket' @::@ 'Text'
--
-- * 'cp1Name' @::@ 'Text'
--
-- * 'cp1Notifications' @::@ 'Maybe' 'Notifications'
--
-- * 'cp1OutputBucket' @::@ 'Maybe' 'Text'
--
-- * 'cp1Role' @::@ 'Text'
--
-- * 'cp1ThumbnailConfig' @::@ 'Maybe' 'PipelineOutputConfig'
--
createPipeline :: Text -- ^ 'cp1Name'
               -> Text -- ^ 'cp1InputBucket'
               -> Text -- ^ 'cp1Role'
               -> CreatePipeline
createPipeline p1 p2 p3 = CreatePipeline
    { _cp1Name            = p1
    , _cp1InputBucket     = p2
    , _cp1Role            = p3
    , _cp1OutputBucket    = Nothing
    , _cp1Notifications   = Nothing
    , _cp1ContentConfig   = Nothing
    , _cp1ThumbnailConfig = Nothing
    }

-- | The optional ContentConfig object specifies information about the Amazon
-- S3 bucket in which you want Elastic Transcoder to save transcoded files
-- and playlists: which bucket to use, which users you want to have access
-- to the files, the type of access you want users to have, and the storage
-- class that you want to assign to the files. If you specify values for
-- ContentConfig, you must also specify values for ThumbnailConfig. If you
-- specify values for ContentConfig and ThumbnailConfig, omit the
-- OutputBucket object. Bucket: The Amazon S3 bucket in which you want
-- Elastic Transcoder to save transcoded files and playlists. Permissions
-- (Optional): The Permissions object specifies which users you want to have
-- access to transcoded files and the type of access you want them to have.
-- You can grant permissions to a maximum of 30 users and/or predefined
-- Amazon S3 groups. Grantee Type: Specify the type of value that appears in
-- the Grantee object: Canonical: The value in the Grantee object is either
-- the canonical user ID for an AWS account or an origin access identity for
-- an Amazon CloudFront distribution. For more information about canonical
-- user IDs, see Access Control List (ACL) Overview in the Amazon Simple
-- Storage Service Developer Guide. For more information about using
-- CloudFront origin access identities to require that users use CloudFront
-- URLs instead of Amazon S3 URLs, see Using an Origin Access Identity to
-- Restrict Access to Your Amazon S3 Content. A canonical user ID is not the
-- same as an AWS account number. Email: The value in the Grantee object is
-- the registered email address of an AWS account. Group: The value in the
-- Grantee object is one of the following predefined Amazon S3 groups:
-- AllUsers, AuthenticatedUsers, or LogDelivery. Grantee: The AWS user or
-- group that you want to have access to transcoded files and playlists. To
-- identify the user or group, you can specify the canonical user ID for an
-- AWS account, an origin access identity for a CloudFront distribution, the
-- registered email address of an AWS account, or a predefined Amazon S3
-- group Access: The permission that you want to give to the AWS user that
-- you specified in Grantee. Permissions are granted on the files that
-- Elastic Transcoder adds to the bucket, including playlists and video
-- files. Valid values include: READ: The grantee can read the objects and
-- metadata for objects that Elastic Transcoder adds to the Amazon S3
-- bucket. READ_ACP: The grantee can read the object ACL for objects that
-- Elastic Transcoder adds to the Amazon S3 bucket. WRITE_ACP: The grantee
-- can write the ACL for the objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. FULL_CONTROL: The grantee has READ, READ_ACP, and
-- WRITE_ACP permissions for the objects that Elastic Transcoder adds to the
-- Amazon S3 bucket. StorageClass: The Amazon S3 storage class, Standard or
-- ReducedRedundancy, that you want Elastic Transcoder to assign to the
-- video files and playlists that it stores in your Amazon S3 bucket.
cp1ContentConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cp1ContentConfig = lens _cp1ContentConfig (\s a -> s { _cp1ContentConfig = a })

-- | The Amazon S3 bucket in which you saved the media files that you want to
-- transcode.
cp1InputBucket :: Lens' CreatePipeline Text
cp1InputBucket = lens _cp1InputBucket (\s a -> s { _cp1InputBucket = a })

-- | The name of the pipeline. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced. Constraints: Maximum 40
-- characters.
cp1Name :: Lens' CreatePipeline Text
cp1Name = lens _cp1Name (\s a -> s { _cp1Name = a })

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want
-- to notify to report job status. To receive notifications, you must also
-- subscribe to the new topic in the Amazon SNS console. Progressing: The
-- topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic
-- that you want to notify when Elastic Transcoder has started to process a
-- job in this pipeline. This is the ARN that Amazon SNS returned when you
-- created the topic. For more information, see Create a Topic in the Amazon
-- Simple Notification Service Developer Guide. Completed: The topic ARN for
-- the Amazon SNS topic that you want to notify when Elastic Transcoder has
-- finished processing a job in this pipeline. This is the ARN that Amazon
-- SNS returned when you created the topic. Warning: The topic ARN for the
-- Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters a warning condition while processing a job in this pipeline.
-- This is the ARN that Amazon SNS returned when you created the topic.
-- Error: The topic ARN for the Amazon SNS topic that you want to notify
-- when Elastic Transcoder encounters an error condition while processing a
-- job in this pipeline. This is the ARN that Amazon SNS returned when you
-- created the topic.
cp1Notifications :: Lens' CreatePipeline (Maybe Notifications)
cp1Notifications = lens _cp1Notifications (\s a -> s { _cp1Notifications = a })

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. (Use this, or use ContentConfig:Bucket plus
-- ThumbnailConfig:Bucket.) Specify this value when all of the following are
-- true: You want to save transcoded files, thumbnails (if any), and
-- playlists (if any) together in one bucket. You do not want to specify the
-- users or groups who have access to the transcoded files, thumbnails, and
-- playlists. You do not want to specify the permissions that Elastic
-- Transcoder grants to the files. When Elastic Transcoder saves files in
-- OutputBucket, it grants full control over the files only to the AWS
-- account that owns the role that is specified by Role. You want to
-- associate the transcoded files and thumbnails with the Amazon S3 Standard
-- storage class. If you want to save transcoded files and playlists in one
-- bucket and thumbnails in another bucket, specify which users can access
-- the transcoded files or the permissions the users have, or change the
-- Amazon S3 storage class, omit OutputBucket and specify values for
-- ContentConfig and ThumbnailConfig instead.
cp1OutputBucket :: Lens' CreatePipeline (Maybe Text)
cp1OutputBucket = lens _cp1OutputBucket (\s a -> s { _cp1OutputBucket = a })

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to use to create the pipeline.
cp1Role :: Lens' CreatePipeline Text
cp1Role = lens _cp1Role (\s a -> s { _cp1Role = a })

-- | The ThumbnailConfig object specifies several values, including the Amazon
-- S3 bucket in which you want Elastic Transcoder to save thumbnail files,
-- which users you want to have access to the files, the type of access you
-- want users to have, and the storage class that you want to assign to the
-- files. If you specify values for ContentConfig, you must also specify
-- values for ThumbnailConfig even if you don't want to create thumbnails.
-- If you specify values for ContentConfig and ThumbnailConfig, omit the
-- OutputBucket object. Bucket: The Amazon S3 bucket in which you want
-- Elastic Transcoder to save thumbnail files. Permissions (Optional): The
-- Permissions object specifies which users and/or predefined Amazon S3
-- groups you want to have access to thumbnail files, and the type of access
-- you want them to have. You can grant permissions to a maximum of 30 users
-- and/or predefined Amazon S3 groups. GranteeType: Specify the type of
-- value that appears in the Grantee object: Canonical: The value in the
-- Grantee object is either the canonical user ID for an AWS account or an
-- origin access identity for an Amazon CloudFront distribution. A canonical
-- user ID is not the same as an AWS account number. Email: The value in the
-- Grantee object is the registered email address of an AWS account. Group:
-- The value in the Grantee object is one of the following predefined Amazon
-- S3 groups: AllUsers, AuthenticatedUsers, or LogDelivery. Grantee: The AWS
-- user or group that you want to have access to thumbnail files. To
-- identify the user or group, you can specify the canonical user ID for an
-- AWS account, an origin access identity for a CloudFront distribution, the
-- registered email address of an AWS account, or a predefined Amazon S3
-- group. Access: The permission that you want to give to the AWS user that
-- you specified in Grantee. Permissions are granted on the thumbnail files
-- that Elastic Transcoder adds to the bucket. Valid values include: READ:
-- The grantee can read the thumbnails and metadata for objects that Elastic
-- Transcoder adds to the Amazon S3 bucket. READ_ACP: The grantee can read
-- the object ACL for thumbnails that Elastic Transcoder adds to the Amazon
-- S3 bucket. WRITE_ACP: The grantee can write the ACL for the thumbnails
-- that Elastic Transcoder adds to the Amazon S3 bucket. FULL_CONTROL: The
-- grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails
-- that Elastic Transcoder adds to the Amazon S3 bucket. StorageClass: The
-- Amazon S3 storage class, Standard or ReducedRedundancy, that you want
-- Elastic Transcoder to assign to the thumbnails that it stores in your
-- Amazon S3 bucket.
cp1ThumbnailConfig :: Lens' CreatePipeline (Maybe PipelineOutputConfig)
cp1ThumbnailConfig =
    lens _cp1ThumbnailConfig (\s a -> s { _cp1ThumbnailConfig = a })

instance ToPath CreatePipeline where
    toPath = const "/2012-09-25/pipelines"

instance ToQuery CreatePipeline where
    toQuery = const mempty

instance ToHeaders CreatePipeline

instance ToBody CreatePipeline where
    toBody = toBody . encode . _cp1Name

newtype CreatePipelineResponse = CreatePipelineResponse
    { _cprPipeline :: Maybe Pipeline
    } deriving (Eq, Show, Generic)

-- | 'CreatePipelineResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprPipeline' @::@ 'Maybe' 'Pipeline'
--
createPipelineResponse :: CreatePipelineResponse
createPipelineResponse = CreatePipelineResponse
    { _cprPipeline = Nothing
    }

-- | A section of the response body that provides information about the
-- pipeline that is created.
cprPipeline :: Lens' CreatePipelineResponse (Maybe Pipeline)
cprPipeline = lens _cprPipeline (\s a -> s { _cprPipeline = a })

instance AWSRequest CreatePipeline where
    type Sv CreatePipeline = ElasticTranscoder
    type Rs CreatePipeline = CreatePipelineResponse

    request  = post
    response = jsonResponse $ \h o -> CreatePipelineResponse
        <$> o .: "Pipeline"
