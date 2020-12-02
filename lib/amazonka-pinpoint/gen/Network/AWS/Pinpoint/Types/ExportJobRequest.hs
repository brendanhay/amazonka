{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the settings for a job that exports endpoint definitions to an Amazon Simple Storage Service (Amazon S3) bucket.
--
--
--
-- /See:/ 'exportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { _eSegmentId ::
      !(Maybe Text),
    _eSegmentVersion :: !(Maybe Int),
    _eS3URLPrefix :: !Text,
    _eRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportJobRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSegmentId' - The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
--
-- * 'eSegmentVersion' - The version of the segment to export endpoint definitions from, if specified.
--
-- * 'eS3URLPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
--
-- * 'eRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
exportJobRequest ::
  -- | 'eS3URLPrefix'
  Text ->
  -- | 'eRoleARN'
  Text ->
  ExportJobRequest
exportJobRequest pS3URLPrefix_ pRoleARN_ =
  ExportJobRequest'
    { _eSegmentId = Nothing,
      _eSegmentVersion = Nothing,
      _eS3URLPrefix = pS3URLPrefix_,
      _eRoleARN = pRoleARN_
    }

-- | The identifier for the segment to export endpoint definitions from. If you don't specify this value, Amazon Pinpoint exports definitions for all the endpoints that are associated with the application.
eSegmentId :: Lens' ExportJobRequest (Maybe Text)
eSegmentId = lens _eSegmentId (\s a -> s {_eSegmentId = a})

-- | The version of the segment to export endpoint definitions from, if specified.
eSegmentVersion :: Lens' ExportJobRequest (Maybe Int)
eSegmentVersion = lens _eSegmentVersion (\s a -> s {_eSegmentVersion = a})

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3) bucket where you want to export endpoint definitions to. This location is typically a folder that contains multiple files. The URL should be in the following format: s3://<replaceable>bucket-name/<replaceable>folder-name/.
eS3URLPrefix :: Lens' ExportJobRequest Text
eS3URLPrefix = lens _eS3URLPrefix (\s a -> s {_eS3URLPrefix = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3 location where you want to export endpoint definitions to.
eRoleARN :: Lens' ExportJobRequest Text
eRoleARN = lens _eRoleARN (\s a -> s {_eRoleARN = a})

instance Hashable ExportJobRequest

instance NFData ExportJobRequest

instance ToJSON ExportJobRequest where
  toJSON ExportJobRequest' {..} =
    object
      ( catMaybes
          [ ("SegmentId" .=) <$> _eSegmentId,
            ("SegmentVersion" .=) <$> _eSegmentVersion,
            Just ("S3UrlPrefix" .= _eS3URLPrefix),
            Just ("RoleArn" .= _eRoleARN)
          ]
      )
