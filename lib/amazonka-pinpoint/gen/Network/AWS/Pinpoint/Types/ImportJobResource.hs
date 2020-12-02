{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobResource where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import Network.AWS.Prelude

-- | Provides information about the resource settings for a job that imports endpoint definitions from one or more files. The files can be stored in an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly from a computer by using the Amazon Pinpoint console.
--
--
--
-- /See:/ 'importJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
  { _ijrSegmentName ::
      !(Maybe Text),
    _ijrDefineSegment :: !(Maybe Bool),
    _ijrRegisterEndpoints :: !(Maybe Bool),
    _ijrExternalId :: !(Maybe Text),
    _ijrSegmentId :: !(Maybe Text),
    _ijrFormat :: !DefinitionFormat,
    _ijrS3URL :: !Text,
    _ijrRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportJobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijrSegmentName' - The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- * 'ijrDefineSegment' - Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- * 'ijrRegisterEndpoints' - Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- * 'ijrExternalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- * 'ijrSegmentId' - The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
--
-- * 'ijrFormat' - The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- * 'ijrS3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- * 'ijrRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
importJobResource ::
  -- | 'ijrFormat'
  DefinitionFormat ->
  -- | 'ijrS3URL'
  Text ->
  -- | 'ijrRoleARN'
  Text ->
  ImportJobResource
importJobResource pFormat_ pS3URL_ pRoleARN_ =
  ImportJobResource'
    { _ijrSegmentName = Nothing,
      _ijrDefineSegment = Nothing,
      _ijrRegisterEndpoints = Nothing,
      _ijrExternalId = Nothing,
      _ijrSegmentId = Nothing,
      _ijrFormat = pFormat_,
      _ijrS3URL = pS3URL_,
      _ijrRoleARN = pRoleARN_
    }

-- | The custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
ijrSegmentName :: Lens' ImportJobResource (Maybe Text)
ijrSegmentName = lens _ijrSegmentName (\s a -> s {_ijrSegmentName = a})

-- | Specifies whether the import job creates a segment that contains the endpoints, when the endpoint definitions are imported.
ijrDefineSegment :: Lens' ImportJobResource (Maybe Bool)
ijrDefineSegment = lens _ijrDefineSegment (\s a -> s {_ijrDefineSegment = a})

-- | Specifies whether the import job registers the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
ijrRegisterEndpoints :: Lens' ImportJobResource (Maybe Bool)
ijrRegisterEndpoints = lens _ijrRegisterEndpoints (\s a -> s {_ijrRegisterEndpoints = a})

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
ijrExternalId :: Lens' ImportJobResource (Maybe Text)
ijrExternalId = lens _ijrExternalId (\s a -> s {_ijrExternalId = a})

-- | The identifier for the segment that the import job updates or adds endpoint definitions to, if the import job updates an existing segment.
ijrSegmentId :: Lens' ImportJobResource (Maybe Text)
ijrSegmentId = lens _ijrSegmentId (\s a -> s {_ijrSegmentId = a})

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the files are stored in an Amazon S3 location and that location contains multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
ijrFormat :: Lens' ImportJobResource DefinitionFormat
ijrFormat = lens _ijrFormat (\s a -> s {_ijrFormat = a})

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
ijrS3URL :: Lens' ImportJobResource Text
ijrS3URL = lens _ijrS3URL (\s a -> s {_ijrS3URL = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
ijrRoleARN :: Lens' ImportJobResource Text
ijrRoleARN = lens _ijrRoleARN (\s a -> s {_ijrRoleARN = a})

instance FromJSON ImportJobResource where
  parseJSON =
    withObject
      "ImportJobResource"
      ( \x ->
          ImportJobResource'
            <$> (x .:? "SegmentName")
            <*> (x .:? "DefineSegment")
            <*> (x .:? "RegisterEndpoints")
            <*> (x .:? "ExternalId")
            <*> (x .:? "SegmentId")
            <*> (x .: "Format")
            <*> (x .: "S3Url")
            <*> (x .: "RoleArn")
      )

instance Hashable ImportJobResource

instance NFData ImportJobResource
