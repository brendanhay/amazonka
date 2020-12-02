{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportToS3TaskSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportToS3TaskSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.DiskImageFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance export task.
--
--
--
-- /See:/ 'exportToS3TaskSpecification' smart constructor.
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'
  { _etstsContainerFormat ::
      !(Maybe ContainerFormat),
    _etstsS3Prefix :: !(Maybe Text),
    _etstsS3Bucket :: !(Maybe Text),
    _etstsDiskImageFormat ::
      !(Maybe DiskImageFormat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportToS3TaskSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etstsContainerFormat' - The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- * 'etstsS3Prefix' - The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
--
-- * 'etstsS3Bucket' - The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- * 'etstsDiskImageFormat' - The format for the exported image.
exportToS3TaskSpecification ::
  ExportToS3TaskSpecification
exportToS3TaskSpecification =
  ExportToS3TaskSpecification'
    { _etstsContainerFormat = Nothing,
      _etstsS3Prefix = Nothing,
      _etstsS3Bucket = Nothing,
      _etstsDiskImageFormat = Nothing
    }

-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat = lens _etstsContainerFormat (\s a -> s {_etstsContainerFormat = a})

-- | The image is written to a single object in the Amazon S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\s a -> s {_etstsS3Prefix = a})

-- | The Amazon S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\s a -> s {_etstsS3Bucket = a})

-- | The format for the exported image.
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat = lens _etstsDiskImageFormat (\s a -> s {_etstsDiskImageFormat = a})

instance Hashable ExportToS3TaskSpecification

instance NFData ExportToS3TaskSpecification

instance ToQuery ExportToS3TaskSpecification where
  toQuery ExportToS3TaskSpecification' {..} =
    mconcat
      [ "ContainerFormat" =: _etstsContainerFormat,
        "S3Prefix" =: _etstsS3Prefix,
        "S3Bucket" =: _etstsS3Bucket,
        "DiskImageFormat" =: _etstsDiskImageFormat
      ]
