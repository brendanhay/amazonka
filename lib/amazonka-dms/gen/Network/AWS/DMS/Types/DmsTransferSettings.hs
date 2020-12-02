{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DmsTransferSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsTransferSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
--
--
-- /See:/ 'dmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { _dtsServiceAccessRoleARN ::
      !(Maybe Text),
    _dtsBucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DmsTransferSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsServiceAccessRoleARN' - The IAM role that has permission to access the Amazon S3 bucket.
--
-- * 'dtsBucketName' - The name of the S3 bucket to use.
dmsTransferSettings ::
  DmsTransferSettings
dmsTransferSettings =
  DmsTransferSettings'
    { _dtsServiceAccessRoleARN = Nothing,
      _dtsBucketName = Nothing
    }

-- | The IAM role that has permission to access the Amazon S3 bucket.
dtsServiceAccessRoleARN :: Lens' DmsTransferSettings (Maybe Text)
dtsServiceAccessRoleARN = lens _dtsServiceAccessRoleARN (\s a -> s {_dtsServiceAccessRoleARN = a})

-- | The name of the S3 bucket to use.
dtsBucketName :: Lens' DmsTransferSettings (Maybe Text)
dtsBucketName = lens _dtsBucketName (\s a -> s {_dtsBucketName = a})

instance FromJSON DmsTransferSettings where
  parseJSON =
    withObject
      "DmsTransferSettings"
      ( \x ->
          DmsTransferSettings'
            <$> (x .:? "ServiceAccessRoleArn") <*> (x .:? "BucketName")
      )

instance Hashable DmsTransferSettings

instance NFData DmsTransferSettings

instance ToJSON DmsTransferSettings where
  toJSON DmsTransferSettings' {..} =
    object
      ( catMaybes
          [ ("ServiceAccessRoleArn" .=) <$> _dtsServiceAccessRoleARN,
            ("BucketName" .=) <$> _dtsBucketName
          ]
      )
