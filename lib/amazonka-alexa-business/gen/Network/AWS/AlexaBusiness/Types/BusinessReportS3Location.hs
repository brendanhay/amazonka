{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportS3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The S3 location of the output reports.
--
--
--
-- /See:/ 'businessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { _brslPath ::
      !(Maybe Text),
    _brslBucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BusinessReportS3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brslPath' - The path of the business report.
--
-- * 'brslBucketName' - The S3 bucket name of the output reports.
businessReportS3Location ::
  BusinessReportS3Location
businessReportS3Location =
  BusinessReportS3Location'
    { _brslPath = Nothing,
      _brslBucketName = Nothing
    }

-- | The path of the business report.
brslPath :: Lens' BusinessReportS3Location (Maybe Text)
brslPath = lens _brslPath (\s a -> s {_brslPath = a})

-- | The S3 bucket name of the output reports.
brslBucketName :: Lens' BusinessReportS3Location (Maybe Text)
brslBucketName = lens _brslBucketName (\s a -> s {_brslBucketName = a})

instance FromJSON BusinessReportS3Location where
  parseJSON =
    withObject
      "BusinessReportS3Location"
      ( \x ->
          BusinessReportS3Location'
            <$> (x .:? "Path") <*> (x .:? "BucketName")
      )

instance Hashable BusinessReportS3Location

instance NFData BusinessReportS3Location
