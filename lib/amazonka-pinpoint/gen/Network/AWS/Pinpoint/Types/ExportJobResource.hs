{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the resource settings for a job that exports endpoint definitions to a file. The file can be added directly to an Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon Pinpoint API or downloaded directly to a computer by using the Amazon Pinpoint console.
--
--
--
-- /See:/ 'exportJobResource' smart constructor.
data ExportJobResource = ExportJobResource'
  { _ejrSegmentId ::
      !(Maybe Text),
    _ejrSegmentVersion :: !(Maybe Int),
    _ejrS3URLPrefix :: !Text,
    _ejrRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportJobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejrSegmentId' - The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
--
-- * 'ejrSegmentVersion' - The version of the segment that the endpoint definitions were exported from.
--
-- * 'ejrS3URLPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- * 'ejrRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
exportJobResource ::
  -- | 'ejrS3URLPrefix'
  Text ->
  -- | 'ejrRoleARN'
  Text ->
  ExportJobResource
exportJobResource pS3URLPrefix_ pRoleARN_ =
  ExportJobResource'
    { _ejrSegmentId = Nothing,
      _ejrSegmentVersion = Nothing,
      _ejrS3URLPrefix = pS3URLPrefix_,
      _ejrRoleARN = pRoleARN_
    }

-- | The identifier for the segment that the endpoint definitions were exported from. If this value isn't present, Amazon Pinpoint exported definitions for all the endpoints that are associated with the application.
ejrSegmentId :: Lens' ExportJobResource (Maybe Text)
ejrSegmentId = lens _ejrSegmentId (\s a -> s {_ejrSegmentId = a})

-- | The version of the segment that the endpoint definitions were exported from.
ejrSegmentVersion :: Lens' ExportJobResource (Maybe Int)
ejrSegmentVersion = lens _ejrSegmentVersion (\s a -> s {_ejrSegmentVersion = a})

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where the endpoint definitions were exported to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
ejrS3URLPrefix :: Lens' ExportJobResource Text
ejrS3URLPrefix = lens _ejrS3URLPrefix (\s a -> s {_ejrS3URLPrefix = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location where the endpoint definitions were exported to.
ejrRoleARN :: Lens' ExportJobResource Text
ejrRoleARN = lens _ejrRoleARN (\s a -> s {_ejrRoleARN = a})

instance FromJSON ExportJobResource where
  parseJSON =
    withObject
      "ExportJobResource"
      ( \x ->
          ExportJobResource'
            <$> (x .:? "SegmentId")
            <*> (x .:? "SegmentVersion")
            <*> (x .: "S3UrlPrefix")
            <*> (x .: "RoleArn")
      )

instance Hashable ExportJobResource

instance NFData ExportJobResource
