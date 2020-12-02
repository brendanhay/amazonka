{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentImportResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentImportResource where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import Network.AWS.Prelude

-- | Provides information about the import job that created a segment. An import job is a job that creates a user segment by importing endpoint definitions.
--
--
--
-- /See:/ 'segmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { _sirChannelCounts ::
      !(Maybe (Map Text (Int))),
    _sirFormat :: !DefinitionFormat,
    _sirS3URL :: !Text,
    _sirSize :: !Int,
    _sirExternalId :: !Text,
    _sirRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentImportResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirChannelCounts' - The number of channel types in the endpoint definitions that were imported to create the segment.
--
-- * 'sirFormat' - The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
--
-- * 'sirS3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
--
-- * 'sirSize' - The number of endpoint definitions that were imported successfully to create the segment.
--
-- * 'sirExternalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- * 'sirRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
segmentImportResource ::
  -- | 'sirFormat'
  DefinitionFormat ->
  -- | 'sirS3URL'
  Text ->
  -- | 'sirSize'
  Int ->
  -- | 'sirExternalId'
  Text ->
  -- | 'sirRoleARN'
  Text ->
  SegmentImportResource
segmentImportResource
  pFormat_
  pS3URL_
  pSize_
  pExternalId_
  pRoleARN_ =
    SegmentImportResource'
      { _sirChannelCounts = Nothing,
        _sirFormat = pFormat_,
        _sirS3URL = pS3URL_,
        _sirSize = pSize_,
        _sirExternalId = pExternalId_,
        _sirRoleARN = pRoleARN_
      }

-- | The number of channel types in the endpoint definitions that were imported to create the segment.
sirChannelCounts :: Lens' SegmentImportResource (HashMap Text (Int))
sirChannelCounts = lens _sirChannelCounts (\s a -> s {_sirChannelCounts = a}) . _Default . _Map

-- | The format of the files that were imported to create the segment. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format.
sirFormat :: Lens' SegmentImportResource DefinitionFormat
sirFormat = lens _sirFormat (\s a -> s {_sirFormat = a})

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that the endpoint definitions were imported from to create the segment.
sirS3URL :: Lens' SegmentImportResource Text
sirS3URL = lens _sirS3URL (\s a -> s {_sirS3URL = a})

-- | The number of endpoint definitions that were imported successfully to create the segment.
sirSize :: Lens' SegmentImportResource Int
sirSize = lens _sirSize (\s a -> s {_sirSize = a})

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
sirExternalId :: Lens' SegmentImportResource Text
sirExternalId = lens _sirExternalId (\s a -> s {_sirExternalId = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorized Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
sirRoleARN :: Lens' SegmentImportResource Text
sirRoleARN = lens _sirRoleARN (\s a -> s {_sirRoleARN = a})

instance FromJSON SegmentImportResource where
  parseJSON =
    withObject
      "SegmentImportResource"
      ( \x ->
          SegmentImportResource'
            <$> (x .:? "ChannelCounts" .!= mempty)
            <*> (x .: "Format")
            <*> (x .: "S3Url")
            <*> (x .: "Size")
            <*> (x .: "ExternalId")
            <*> (x .: "RoleArn")
      )

instance Hashable SegmentImportResource

instance NFData SegmentImportResource
