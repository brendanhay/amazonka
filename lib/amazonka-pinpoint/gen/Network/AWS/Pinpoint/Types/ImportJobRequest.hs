{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DefinitionFormat
import Network.AWS.Prelude

-- | Specifies the settings for a job that imports endpoint definitions from an Amazon Simple Storage Service (Amazon S3) bucket.
--
--
--
-- /See:/ 'importJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
  { _iSegmentName ::
      !(Maybe Text),
    _iDefineSegment :: !(Maybe Bool),
    _iRegisterEndpoints :: !(Maybe Bool),
    _iExternalId :: !(Maybe Text),
    _iSegmentId :: !(Maybe Text),
    _iFormat :: !DefinitionFormat,
    _iS3URL :: !Text,
    _iRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportJobRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iSegmentName' - A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
--
-- * 'iDefineSegment' - Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
--
-- * 'iRegisterEndpoints' - Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
--
-- * 'iExternalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
--
-- * 'iSegmentId' - The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
--
-- * 'iFormat' - The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
--
-- * 'iS3URL' - The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
--
-- * 'iRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
importJobRequest ::
  -- | 'iFormat'
  DefinitionFormat ->
  -- | 'iS3URL'
  Text ->
  -- | 'iRoleARN'
  Text ->
  ImportJobRequest
importJobRequest pFormat_ pS3URL_ pRoleARN_ =
  ImportJobRequest'
    { _iSegmentName = Nothing,
      _iDefineSegment = Nothing,
      _iRegisterEndpoints = Nothing,
      _iExternalId = Nothing,
      _iSegmentId = Nothing,
      _iFormat = pFormat_,
      _iS3URL = pS3URL_,
      _iRoleARN = pRoleARN_
    }

-- | A custom name for the segment that's created by the import job, if the value of the DefineSegment property is true.
iSegmentName :: Lens' ImportJobRequest (Maybe Text)
iSegmentName = lens _iSegmentName (\s a -> s {_iSegmentName = a})

-- | Specifies whether to create a segment that contains the endpoints, when the endpoint definitions are imported.
iDefineSegment :: Lens' ImportJobRequest (Maybe Bool)
iDefineSegment = lens _iDefineSegment (\s a -> s {_iDefineSegment = a})

-- | Specifies whether to register the endpoints with Amazon Pinpoint, when the endpoint definitions are imported.
iRegisterEndpoints :: Lens' ImportJobRequest (Maybe Bool)
iRegisterEndpoints = lens _iRegisterEndpoints (\s a -> s {_iRegisterEndpoints = a})

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID key in an IAM trust policy. Amazon Pinpoint previously used this value to assume an IAM role when importing endpoint definitions, but we removed this requirement. We don't recommend use of external IDs for IAM roles that are assumed by Amazon Pinpoint.
iExternalId :: Lens' ImportJobRequest (Maybe Text)
iExternalId = lens _iExternalId (\s a -> s {_iExternalId = a})

-- | The identifier for the segment to update or add the imported endpoint definitions to, if the import job is meant to update an existing segment.
iSegmentId :: Lens' ImportJobRequest (Maybe Text)
iSegmentId = lens _iSegmentId (\s a -> s {_iSegmentId = a})

-- | The format of the files that contain the endpoint definitions to import. Valid values are: CSV, for comma-separated values format; and, JSON, for newline-delimited JSON format. If the Amazon S3 location stores multiple files that use different formats, Amazon Pinpoint imports data only from the files that use the specified format.
iFormat :: Lens' ImportJobRequest DefinitionFormat
iFormat = lens _iFormat (\s a -> s {_iFormat = a})

-- | The URL of the Amazon Simple Storage Service (Amazon S3) bucket that contains the endpoint definitions to import. This location can be a folder or a single file. If the location is a folder, Amazon Pinpoint imports endpoint definitions from the files in this location, including any subfolders that the folder contains. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/<replaceable>file-name. The location can end with the key for an individual object or a prefix that qualifies multiple objects.
iS3URL :: Lens' ImportJobRequest Text
iS3URL = lens _iS3URL (\s a -> s {_iS3URL = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location to import endpoint definitions from.
iRoleARN :: Lens' ImportJobRequest Text
iRoleARN = lens _iRoleARN (\s a -> s {_iRoleARN = a})

instance Hashable ImportJobRequest

instance NFData ImportJobRequest

instance ToJSON ImportJobRequest where
  toJSON ImportJobRequest' {..} =
    object
      ( catMaybes
          [ ("SegmentName" .=) <$> _iSegmentName,
            ("DefineSegment" .=) <$> _iDefineSegment,
            ("RegisterEndpoints" .=) <$> _iRegisterEndpoints,
            ("ExternalId" .=) <$> _iExternalId,
            ("SegmentId" .=) <$> _iSegmentId,
            Just ("Format" .= _iFormat),
            Just ("S3Url" .= _iS3URL),
            Just ("RoleArn" .= _iRoleARN)
          ]
      )
