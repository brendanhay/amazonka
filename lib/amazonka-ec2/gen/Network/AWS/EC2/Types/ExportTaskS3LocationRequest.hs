{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3LocationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3LocationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the destination for an export image task.
--
--
--
-- /See:/ 'exportTaskS3LocationRequest' smart constructor.
data ExportTaskS3LocationRequest = ExportTaskS3LocationRequest'
  { _etslrS3Prefix ::
      !(Maybe Text),
    _etslrS3Bucket :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTaskS3LocationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etslrS3Prefix' - The prefix (logical hierarchy) in the bucket.
--
-- * 'etslrS3Bucket' - The destination Amazon S3 bucket.
exportTaskS3LocationRequest ::
  -- | 'etslrS3Bucket'
  Text ->
  ExportTaskS3LocationRequest
exportTaskS3LocationRequest pS3Bucket_ =
  ExportTaskS3LocationRequest'
    { _etslrS3Prefix = Nothing,
      _etslrS3Bucket = pS3Bucket_
    }

-- | The prefix (logical hierarchy) in the bucket.
etslrS3Prefix :: Lens' ExportTaskS3LocationRequest (Maybe Text)
etslrS3Prefix = lens _etslrS3Prefix (\s a -> s {_etslrS3Prefix = a})

-- | The destination Amazon S3 bucket.
etslrS3Bucket :: Lens' ExportTaskS3LocationRequest Text
etslrS3Bucket = lens _etslrS3Bucket (\s a -> s {_etslrS3Bucket = a})

instance Hashable ExportTaskS3LocationRequest

instance NFData ExportTaskS3LocationRequest

instance ToQuery ExportTaskS3LocationRequest where
  toQuery ExportTaskS3LocationRequest' {..} =
    mconcat
      ["S3Prefix" =: _etslrS3Prefix, "S3Bucket" =: _etslrS3Bucket]
