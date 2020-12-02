{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.DestinationSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.DestinationSchema where

import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the data format when records are written to the destination. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
--
--
-- /See:/ 'destinationSchema' smart constructor.
newtype DestinationSchema = DestinationSchema'
  { _dsRecordFormatType ::
      RecordFormatType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsRecordFormatType' - Specifies the format of the records on the output stream.
destinationSchema ::
  -- | 'dsRecordFormatType'
  RecordFormatType ->
  DestinationSchema
destinationSchema pRecordFormatType_ =
  DestinationSchema' {_dsRecordFormatType = pRecordFormatType_}

-- | Specifies the format of the records on the output stream.
dsRecordFormatType :: Lens' DestinationSchema RecordFormatType
dsRecordFormatType = lens _dsRecordFormatType (\s a -> s {_dsRecordFormatType = a})

instance FromJSON DestinationSchema where
  parseJSON =
    withObject
      "DestinationSchema"
      (\x -> DestinationSchema' <$> (x .: "RecordFormatType"))

instance Hashable DestinationSchema

instance NFData DestinationSchema

instance ToJSON DestinationSchema where
  toJSON DestinationSchema' {..} =
    object
      (catMaybes [Just ("RecordFormatType" .= _dsRecordFormatType)])
