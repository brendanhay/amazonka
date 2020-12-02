{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3Location where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the destination for an export image task.
--
--
--
-- /See:/ 'exportTaskS3Location' smart constructor.
data ExportTaskS3Location = ExportTaskS3Location'
  { _etslS3Prefix ::
      !(Maybe Text),
    _etslS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTaskS3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etslS3Prefix' - The prefix (logical hierarchy) in the bucket.
--
-- * 'etslS3Bucket' - The destination Amazon S3 bucket.
exportTaskS3Location ::
  ExportTaskS3Location
exportTaskS3Location =
  ExportTaskS3Location'
    { _etslS3Prefix = Nothing,
      _etslS3Bucket = Nothing
    }

-- | The prefix (logical hierarchy) in the bucket.
etslS3Prefix :: Lens' ExportTaskS3Location (Maybe Text)
etslS3Prefix = lens _etslS3Prefix (\s a -> s {_etslS3Prefix = a})

-- | The destination Amazon S3 bucket.
etslS3Bucket :: Lens' ExportTaskS3Location (Maybe Text)
etslS3Bucket = lens _etslS3Bucket (\s a -> s {_etslS3Bucket = a})

instance FromXML ExportTaskS3Location where
  parseXML x =
    ExportTaskS3Location'
      <$> (x .@? "s3Prefix") <*> (x .@? "s3Bucket")

instance Hashable ExportTaskS3Location

instance NFData ExportTaskS3Location
