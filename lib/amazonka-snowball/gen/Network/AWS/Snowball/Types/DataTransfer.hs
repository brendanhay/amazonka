{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DataTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DataTransfer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
--
--
-- /See:/ 'dataTransfer' smart constructor.
data DataTransfer = DataTransfer'
  { _dtTotalObjects ::
      !(Maybe Integer),
    _dtTotalBytes :: !(Maybe Integer),
    _dtObjectsTransferred :: !(Maybe Integer),
    _dtBytesTransferred :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTotalObjects' - The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- * 'dtTotalBytes' - The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- * 'dtObjectsTransferred' - The number of objects transferred between a Snow device and Amazon S3.
--
-- * 'dtBytesTransferred' - The number of bytes transferred between a Snow device and Amazon S3.
dataTransfer ::
  DataTransfer
dataTransfer =
  DataTransfer'
    { _dtTotalObjects = Nothing,
      _dtTotalBytes = Nothing,
      _dtObjectsTransferred = Nothing,
      _dtBytesTransferred = Nothing
    }

-- | The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
dtTotalObjects :: Lens' DataTransfer (Maybe Integer)
dtTotalObjects = lens _dtTotalObjects (\s a -> s {_dtTotalObjects = a})

-- | The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
dtTotalBytes :: Lens' DataTransfer (Maybe Integer)
dtTotalBytes = lens _dtTotalBytes (\s a -> s {_dtTotalBytes = a})

-- | The number of objects transferred between a Snow device and Amazon S3.
dtObjectsTransferred :: Lens' DataTransfer (Maybe Integer)
dtObjectsTransferred = lens _dtObjectsTransferred (\s a -> s {_dtObjectsTransferred = a})

-- | The number of bytes transferred between a Snow device and Amazon S3.
dtBytesTransferred :: Lens' DataTransfer (Maybe Integer)
dtBytesTransferred = lens _dtBytesTransferred (\s a -> s {_dtBytesTransferred = a})

instance FromJSON DataTransfer where
  parseJSON =
    withObject
      "DataTransfer"
      ( \x ->
          DataTransfer'
            <$> (x .:? "TotalObjects")
            <*> (x .:? "TotalBytes")
            <*> (x .:? "ObjectsTransferred")
            <*> (x .:? "BytesTransferred")
      )

instance Hashable DataTransfer

instance NFData DataTransfer
